module Database.Mongodb.Connection
	( MongodbHost, MongodbPort
	, MongodbConnectionInfo(..)
	) where

import Data.Word (Word16)

import Database.Mongodb.Internal (StrictByteString)

type MongodbHost = StrictByteString
type MongodbPort = Word16

data MongodbConnectionInfo = MongodbConnectionInfoInet MongodbHost MongodbPort
						   | MongodbConnectionInfoUnix StrictByteString
