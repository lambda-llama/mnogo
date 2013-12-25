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

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar,
                                withMVar, putMVar)
import Control.Exception (bracket)
import Control.Monad (unless, void)
import Data.Binary (decode)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Word (Word16)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as StrictByteString

import Network.Socket.ByteString.Lazy (sendAll, recv)
import qualified Network.Socket as Socket

import Database.Mongodb.Internal (StrictByteString,
                                  RequestIdCounter, ObjectIdCounter,
                                  newRequestIdCounter, newObjectIdCounter,
                                  newRequestId)
import Database.Mongodb.Protocol (Request, RequestId, Reply,
                                  getInt32, getReply,
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
                 , conSocket      :: !Socket.Socket
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
  conRidCounter  <- newRequestIdCounter
  conOidCounter  <- newObjectIdCounter
  conRequestMVar <- newMVar undefined
  conReplyMapRef <- newIORef Map.empty
  conReplyReader <- forkIO $ replyReader conSocket conReplyMapRef
  return Connection { .. }

close :: Connection -> IO ()
close (Connection { conSocket }) = Socket.close conSocket

withConnection :: ConnectionInfo -> (Connection -> IO a) -> IO a
withConnection info = bracket (connect info) close


-- A work-in-progress prototype of MongoDB "pipe".

replyReader :: Socket.Socket -> IORef (Map RequestId (MVar Reply)) -> IO ()
replyReader socket replyMapRef = do
    -- FIXME(lebedev): this is unsafe, since 'fail == error' in the
    -- IO monad.
    messageLength <- decode <$> recv socket 4
    message <- recv socket messageLength
    -- FIXME(lebedev): this is of course the _meat_ from '.Protocol',
    -- but unfortunately we _need_ the socket to be able to
    -- parse MongoDB reply, which has variable size. One option of
    -- "fixing" this is to:
    --    1. read message header which has fixed size 'sizeof(int32) * 4',
    --    2. run the header through 'Get',
    --    3. read message body (we know the exact size from the header),
    --    4. run 'Get' on the body
    -- Thoughts?
    let (requestId, reply) = flip runGet message $ do
            void getInt32  -- should be the same as 'requestId'
            requestId <- getInt32
            opCode    <- getInt32
            unless (opCode == OP_REPLY) $
                fail $ "getReplyMessage: Expected OP_REPLY, got " <> show opCode
            (requestId, ) <$> getReply
    replyMap <- readIORef replyMapRef
    case Map.lookup requestId replyMap of
        Just replyMVar -> putMVar replyMVar reply
        Nothing -> return ()  -- Ignore unknown requests.

sendRequest :: Connection -> Request -> IO (MVar Reply)
sendRequest (Connection { .. }) request = do
    requestId <- newRequestId conRidCounter
    replyMVar <- newEmptyMVar
    withMVar conRequestMVar $ \_ -> do
        sendAll conSocket (runPut $ putRequestMessage (requestId, request))
        atomicModifyIORef' conReplyMapRef $ \m ->
            (Map.insert requestId replyMVar m, ())
        return replyMVar
