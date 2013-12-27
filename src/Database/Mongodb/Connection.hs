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
import Data.Word (Word16)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as StrictByteString
import qualified System.IO as IO

import Data.Tagged (Tagged, untag)
import qualified Network.Socket as Socket

import Database.Mongodb.Internal (StrictByteString,
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

sendRequestWithReply :: forall rq. (Request rq, ReplyExpected rq ~ True)
                     => Connection -> rq -> IO (MVar Reply)
sendRequestWithReply connection@(Connection { .. }) request = do
    replyMVar <- newEmptyMVar
    withMVar conRequestMVar $ \_ -> do
        requestId <- sendRequest connection request
        atomicModifyIORef' conReplyMapRef $ \m ->
            (Map.insert requestId replyMVar m, ())
    return replyMVar

sendRequestNoReply :: forall rq. (Request rq, ReplyExpected rq ~ False)
                   => Connection -> rq -> IO ()
sendRequestNoReply connection@(Connection { .. }) request = do
    withMVar conRequestMVar $ \_ -> do
        void $ sendRequest connection request

sendRequest :: forall rq. Request rq => Connection -> rq -> IO RequestId
sendRequest (Connection { .. }) request = do
    rhRequestId <- newRequestId conRidCounter
    let body     = runPut $ putRequest request
        bodySize = fromIntegral $ LazyByteString.length body
        rhMessageLength = bodySize + headerSize
        rhOpCode = untag (opCode :: Tagged rq OpCode)
        header   = MessageHeader { rhResponseTo = 0, .. }
    LazyByteString.hPut conHandle $ runPut $ putMessageHeader header
    LazyByteString.hPut conHandle body
    return rhRequestId
  where
    headerSize = 4 * 4  -- sizeof(int32) * 4.
