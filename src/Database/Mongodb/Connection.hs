{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Mongodb.Connection
  ( Host, Port
  , ConnectionInfo(..)
  , Connection(..)
  , connect, close
  , withConnection
  ) where

import Control.Exception (bracket)
import Data.Word (Word16)
import qualified Data.ByteString.Char8 as StrictByteString

import qualified Network.Socket as Socket

import Database.Mongodb.Internal (StrictByteString,
                                  RequestIdCounter, newRequestIdCounter,
                                  ObjectIdCounter, newObjectIdCounter)

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
data Connection = Connection { conRidCounter :: !RequestIdCounter
                             , conOidCounter :: !ObjectIdCounter
                             , conSocket     :: !Socket.Socket
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
  conRidCounter <- newRequestIdCounter
  conOidCounter <- newObjectIdCounter
  return Connection { .. }

close :: Connection -> IO ()
close (Connection { conSocket }) = Socket.close conSocket

withConnection :: ConnectionInfo -> (Connection -> IO a) -> IO a
withConnection info = bracket (connect info) close
