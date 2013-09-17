module Database.Mongodb.Connection
	( MongodbHost, MongodbPort
	, MongodbConnectionInfo(..)
	, MongodbConnection(..)
	, mongodbConnect, mongodbClose
	, withMonodbConnection
	) where

import Data.Word (Word16)

import Control.Monad.Catch (MonadCatch, bracket)
import Control.Monad.Trans (MonadIO(liftIO))

import Database.Mongodb.Internal (StrictByteString)

type MongodbHost = StrictByteString
type MongodbPort = Word16

data MongodbConnectionInfo = MongodbConnectionInfoInet {-# UNPACK #-} !MongodbHost {-# UNPACK #-} !MongodbPort
						   | MongodbConnectionInfoUnix {-# UNPACK #-} !StrictByteString

data MongodbConnection = MongodbConnection

mongodbConnect :: MongodbConnectionInfo -> IO MongodbConnection
mongodbConnect = error "mongodbConnect: not implemented"

mongodbClose :: MongodbConnection -> IO ()
mongodbClose = error "mongodbClose: not implemented"

withMonodbConnection :: (MonadCatch m, MonadIO m) => MongodbConnectionInfo -> (MongodbConnection -> m a) -> m a
withMonodbConnection info = bracket (liftIO $ mongodbConnect info) (liftIO . mongodbClose)
