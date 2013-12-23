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

import Database.Mongodb.Internal (StrictByteString)

type Host = StrictByteString
type Port = Word16

data ConnectionInfo = ConnectionInfoInet {-# UNPACK #-} !Host {-# UNPACK #-} !Port
                    | ConnectionInfoUnix {-# UNPACK #-} !StrictByteString

data Connection = Connection Socket.Socket

connect :: ConnectionInfo -> IO Connection
connect (ConnectionInfoInet host port) = do
  (Socket.AddrInfo { .. }:_) <- Socket.getAddrInfo Nothing
                           (Just $ StrictByteString.unpack host)
                           (Just $ show port)
  socket <- Socket.socket addrFamily addrSocketType addrProtocol
  Socket.connect socket addrAddress
  return $ Connection socket
connect (ConnectionInfoUnix path) = do
  socket <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.connect socket $ Socket.SockAddrUnix $ StrictByteString.unpack path
  return $ Connection socket

close :: Connection -> IO ()
close (Connection socket) = Socket.close socket

withConnection :: ConnectionInfo -> (Connection -> IO a) -> IO a
withConnection info = bracket (connect info) close
