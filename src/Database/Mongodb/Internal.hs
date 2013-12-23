module Database.Mongodb.Internal
  ( StrictByteString
  , RequestIdCounter(..)
  , ObjectIdCounter(..)
  , newRequestIdCounter
  , newObjectIdCounter
  ) where

import Data.Int (Int32)
import Data.IORef (IORef, newIORef)
import System.Random (Random(..), randomIO)
import qualified Data.ByteString.Char8 as StrictByteString

import Data.Word.Word24 (Word24)

type StrictByteString = StrictByteString.ByteString

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
