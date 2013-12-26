{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Monad (forever)
import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar,
                                withMVar, putMVar)
import Control.Exception (bracket)
import Data.Binary (decode)
import Data.Binary.Put (runPut)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import Data.Word (Word16)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as StrictByteString
import qualified System.IO as IO

import qualified Network.Socket as Socket

import Database.Mongodb.Internal (StrictByteString,
                                  RequestIdCounter, ObjectIdCounter,
                                  newRequestIdCounter, newObjectIdCounter,
                                  newRequestId)
import Database.Mongodb.Protocol (Request, RequestId, MessageHeader(..), Reply,
                                  putRequestMessage)

type Host = StrictByteString
type Port = Word16  -- FIXME(lebedev): why only 16 bits?

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
                 , conRequestMVar :: MVar Bool
                 , conReplyMapRef :: IORef (Map RequestId (MVar Reply))
                 , conReplyReader :: ThreadId
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
replyReader :: IO.Handle -> IORef (Map RequestId (MVar Reply)) -> IO ()
replyReader h replyMapRef = forever $ do
    -- FIXME(lebedev): this is unsafe, since 'fail == error' in the
    -- IO monad.
    (MessageHeader { .. }) <- decode <$> LazyByteString.hGet h headerSize
    reply <- decode <$> LazyByteString.hGet h (fromIntegral rhSize - headerSize)
    replyMap <- readIORef replyMapRef
    case Map.lookup rhResponseTo replyMap of
        Just replyMVar -> putMVar replyMVar reply
        Nothing -> return ()  -- Ignore unknown requests.
  where
    headerSize = 4 * 4  -- sizeof(int32) * 4.

sendRequest :: Connection -> Request -> IO (MVar Reply)
sendRequest (Connection { .. }) request = do
    requestId <- newRequestId conRidCounter
    replyMVar <- newEmptyMVar
    withMVar conRequestMVar $ \_ -> do
        LazyByteString.hPut conHandle . runPut $
            putRequestMessage (requestId, request)
        atomicModifyIORef' conReplyMapRef $ \m ->
            (Map.insert requestId replyMVar m, ())
        return replyMVar
