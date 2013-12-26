{-# LANGUAGE RecordWildCards #-}

module Database.Mongodb.Internal
    ( StrictByteString
    , LazyByteString
    , RequestIdCounter(..)
    , ObjectIdCounter(..)
    , newRequestIdCounter
    , newObjectIdCounter
    , newRequestId
    , newObjectId
    ) where

import Data.Int (Int32)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Posix.Process (getProcessID)
import System.Random (Random(..), randomIO)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Char8 as StrictByteString

import Data.Bson (ObjectId(..))
import Data.Word.Word24 (Word24)

type StrictByteString = StrictByteString.ByteString
type LazyByteString = LazyByteString.ByteString

newtype RequestIdCounter = RequestIdCounter { unRequestIdCounter :: IORef Int32 }
newtype ObjectIdCounter = ObjectIdCounter { unObjectIdCounter :: IORef Word24 }

instance Random Word24 where
    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
        (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

newRequestIdCounter :: IO RequestIdCounter
newRequestIdCounter = fmap RequestIdCounter $ newIORef 0

newObjectIdCounter :: IO ObjectIdCounter
newObjectIdCounter = fmap ObjectIdCounter $ randomIO >>= newIORef

newRequestId :: RequestIdCounter -> IO Int32
newRequestId (RequestIdCounter counterRef) = atomicModifyIORef' counterRef $ \r -> (r + 1, r)

newObjectId :: ObjectIdCounter -> IO ObjectId
newObjectId (ObjectIdCounter counterRef) = do
    objectIdInc <- atomicModifyIORef' counterRef $ \r -> (r + 1, r)
    objectIdTime <- fmap truncate getPOSIXTime
    objectIdPid <- fmap fromIntegral getProcessID
    objectIdMachine <- return 0
    return $! ObjectId { .. }
