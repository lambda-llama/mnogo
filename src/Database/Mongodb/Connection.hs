{-# OPTIONS_GHC -funbox-strict-fields #-}

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

import Database.Mongodb.Internal (StrictByteString, RequestIdCounter,
                                  ObjectIdCounter, newRequestIdCounter,
                                  newObjectIdCounter)

type Host = StrictByteString
type Port = Word16

data ConnectionInfo = ConnectionInfoInet !Host !Port
                    | ConnectionInfoUnix !StrictByteString

data Connection = Connection !RequestIdCounter !ObjectIdCounter !Socket.Socket

connect :: ConnectionInfo -> IO Connection
connect info = do
  socket <- case info of
    (ConnectionInfoInet host port) -> do
      (Socket.AddrInfo { .. }:_) <- Socket.getAddrInfo Nothing
                               (Just $ StrictByteString.unpack host)
                               (Just $ show port)
      socket <- Socket.socket addrFamily addrSocketType addrProtocol
      Socket.connect socket addrAddress
      return socket
    (ConnectionInfoUnix path) -> do
      socket <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      Socket.connect socket $ Socket.SockAddrUnix $ StrictByteString.unpack path
      return socket
  requestIdCounter <- newRequestIdCounter
  objectIdCounter <- newObjectIdCounter
  return $ Connection requestIdCounter objectIdCounter socket

close :: Connection -> IO ()
close (Connection _ _ socket) = Socket.close socket

withConnection :: ConnectionInfo -> (Connection -> IO a) -> IO a
withConnection info = bracket (connect info) close
