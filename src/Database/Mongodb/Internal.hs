module Database.Mongodb.Internal
    ( StrictByteString
	) where

import qualified Data.ByteString.Char8 as StrictByteString

type StrictByteString = StrictByteString.ByteString
