{-# LANGUAGE RecordWildCards #-}

module Database.Mongodb.Connection
	( MongodbHost, MongodbPort
	, MongodbConnectionInfo(..)
	, MongodbConnection(..)
	, mongodbConnect, mongodbClose
	, withMonodbConnection
	) where

import Data.Word (Word16)
import qualified Data.ByteString.Char8 as StrictByteString

import Control.Monad.Catch (MonadCatch, bracket)
import Control.Monad.Trans (MonadIO(liftIO))
import qualified Network.Socket as Socket

import Database.Mongodb.Internal (StrictByteString)

type MongodbHost = StrictByteString
type MongodbPort = Word16

data MongodbConnectionInfo = MongodbConnectionInfoInet {-# UNPACK #-} !MongodbHost {-# UNPACK #-} !MongodbPort
						   | MongodbConnectionInfoUnix {-# UNPACK #-} !StrictByteString

data MongodbConnection = MongodbConnection Socket.Socket

mongodbConnect :: MongodbConnectionInfo -> IO MongodbConnection
mongodbConnect (MongodbConnectionInfoInet host port) = do
	(Socket.AddrInfo { .. }:_) <- Socket.getAddrInfo Nothing
													 (Just $ StrictByteString.unpack host)
													 (Just $ show port)
	socket <- Socket.socket addrFamily addrSocketType addrProtocol
	Socket.connect socket addrAddress
	return $ MongodbConnection socket
mongodbConnect (MongodbConnectionInfoUnix path) = do
	socket <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
	Socket.connect socket $ Socket.SockAddrUnix $ StrictByteString.unpack path
	return $ MongodbConnection socket

mongodbClose :: MongodbConnection -> IO ()
mongodbClose = error "mongodbClose: not implemented"

withMonodbConnection :: (MonadCatch m, MonadIO m) => MongodbConnectionInfo -> (MongodbConnection -> m a) -> m a
withMonodbConnection info = bracket (liftIO $ mongodbConnect info) (liftIO . mongodbClose)
