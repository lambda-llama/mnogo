{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Database.Mongodb.Connection
  ( Host, Port
  , ConnectionInfo(..)
  , Connection(..)
  , connect, close
  , withConnection
  ) where

#include "protocol.h"

import Control.Monad (forever, void)
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar,
                                withMVar, putMVar)
import Control.Exception (bracket)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Word (Word16)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as StrictByteString
import qualified System.IO as IO

import Data.Tagged (Tagged, untag)
import qualified Network.Socket as Socket

import Database.Mongodb.Internal (StrictByteString, LazyByteString,
                                  RequestIdCounter, ObjectIdCounter,
                                  newRequestIdCounter, newObjectIdCounter,
                                  newRequestId)
import Database.Mongodb.Protocol (Request(..), RequestId, OpCode, MessageHeader(..),
                                  Reply, getMessageHeader, putMessageHeader, getReply)

type Host = StrictByteString
type Port = Word16  -- FIXME(lebedev): why only 16 bits?

type ReplyMapRef = IORef (Map RequestId (MVar Reply))

data ConnectionInfo = ConnectionInfoInet !Host !Port
                    | ConnectionInfoUnix !StrictByteString

{- Note(lebedev): how about a phantom type, representing connection
   state, i. e.

     data Open
     data Closed
     data Connection phantom = ...

   This is a classical phantom types example, but it's still useful one.
  -}
data Connection
    = Connection { conRidCounter  :: !RequestIdCounter
                 , conOidCounter  :: !ObjectIdCounter
                 , conHandle      :: !IO.Handle
                 , conRequestMVar :: !(MVar Bool)
                 , conReplyMapRef :: !ReplyMapRef
                 , conReplyReader :: !ThreadId
                 }

connect :: ConnectionInfo -> IO Connection
connect info = do
   conSocket <- case info of
     ConnectionInfoInet host port -> do
         (Socket.AddrInfo { .. }:_) <- Socket.getAddrInfo Nothing
                                       (Just $ StrictByteString.unpack host)
                                       (Just $ show port)
         socket <- Socket.socket addrFamily addrSocketType addrProtocol
         Socket.connect socket addrAddress
         return socket
     ConnectionInfoUnix path -> do
         socket <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
         Socket.connect socket $ Socket.SockAddrUnix $ StrictByteString.unpack path
         return socket
   conHandle      <- Socket.socketToHandle conSocket IO.ReadWriteMode
   conRidCounter  <- newRequestIdCounter
   conOidCounter  <- newObjectIdCounter
   conRequestMVar <- newMVar undefined
   conReplyMapRef <- newIORef Map.empty
   conReplyReader <- forkIO $ replyReader conHandle conReplyMapRef
   return Connection { .. }

close :: Connection -> IO ()
close (Connection { conHandle, conReplyReader }) = do
    -- FIXME(lebedev): how to deal with unfilled MVars in 'conReplyMap'?
    killThread conReplyReader
    IO.hClose conHandle

withConnection :: ConnectionInfo -> (Connection -> IO a) -> IO a
withConnection info = bracket (connect info) close

-- | A per-connection worker thread, which reads MongoDB messages from
-- the handle and fills @MVar@s for the corresponding @RequestId@s.
replyReader :: IO.Handle -> ReplyMapRef -> IO ()
replyReader h replyMapRef = forever $ do
    -- FIXME(lebedev): this is unsafe, since 'fail == error' in the
    -- IO monad.
    (MessageHeader { .. }) <- runGet getMessageHeader <$>
        LazyByteString.hGet h headerSize
    reply <- runGet getReply <$>
        (LazyByteString.hGet h $ fromIntegral rhMessageLength - headerSize)
    replyMap <- readIORef replyMapRef
    case Map.lookup rhResponseTo replyMap of
        Just replyMVar -> putMVar replyMVar reply
        Nothing -> return ()  -- Ignore unknown requests.
  where
    headerSize = 4 * 4  -- sizeof(int32) * 4.

withRequestLock :: Connection -> IO a -> IO a
withRequestLock Connection { conRequestMVar } =
    withMVar conRequestMVar . const
{-# INLINE withRequestLock #-}

sendRequestWithReply :: forall rq. (Request rq, ReplyExpected rq ~ True)
                     => Connection -> rq -> IO (MVar Reply)
sendRequestWithReply connection@(Connection { conReplyMapRef }) request = do
    replyMVar <- newEmptyMVar
    requestId <- sendRequest connection request
    atomicModifyIORef' conReplyMapRef $ \m ->
        (Map.insert requestId replyMVar m, ())
    return replyMVar
{-# INLINE sendRequestWithReply #-}

sendRequestNoReply :: forall rq. (Request rq, ReplyExpected rq ~ False)
                   => Connection -> rq -> IO ()
sendRequestNoReply connection request =
    void $ sendRequest connection request
{-# INLINE sendRequestNoReply #-}

sendRequest :: forall rq. Request rq => Connection -> rq -> IO RequestId
sendRequest (Connection { .. }) request = do
    requestId <- newRequestId conRidCounter
    LazyByteString.hPut conHandle $ formatRequest request requestId
    return requestId
{-# INLINE sendRequest #-}

formatRequest :: forall rq. Request rq => rq -> RequestId -> LazyByteString
formatRequest request rhRequestId = header <> body
  where
    body = runPut $ putRequest request
    bodySize = fromIntegral $ LazyByteString.length body

    headerSize = 4 * 4  -- sizeof(int32) * 4.
    rhOpCode = untag (opCode :: Tagged rq OpCode)
    rhMessageLength = bodySize + headerSize
    header = runPut $ putMessageHeader $ MessageHeader { rhResponseTo = 0, .. }
{-# INLINE formatRequest #-}
