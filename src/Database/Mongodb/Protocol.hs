module Database.Mongodb.Protocol () where

import Data.Vector (Vector)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Generic.Base as GenericBaseVector
import qualified Data.Vector.Generic.Mutable as GenericMutableVector
import qualified Data.Vector.Unboxed as UnboxedVector

type GenericBaseVector = GenericBaseVector.Vector
type GenericMutableVector = GenericMutableVector.MVector

type UnboxedVector = UnboxedVector.Vector
type UnboxedMutableVector = UnboxedVector.MVector

newtype Database = Database { unDatabase :: Text }
  deriving (Eq, Ord, Show, IsString)

newtype Collection = Collection { unDatabase :: Text }
  deriving (Eq, Ord, Show, IsString)

newtype CursorId = CursorId { unCursorId :: Word64 }
  deriving (Eq, Ord, Show, GenericBaseVector UnboxedVector,
            GenericMutableVector UnboxedMutableVector, Unbox)

data UpdateFlag = Upsert
                | MultiUpdate
  deriving (Eq, Ord, Show)

data InsertFlag = ContinueOnError
  deriving (Eq, Ord, Show)

data DeleteFlag = SingleRemove
  deriving (Eq, Ord, Show)

type UpdateFlags = BitSet UpdateFlag
type InsertFlags = BitSet InsertFlag
type DeleteFlags = BitSet DeleteFlag

data Operation = Reply
        		   | Message {-# UNPACK #-} !Text
        		   | Update {-# UNPACK #-} !Database {-# UNPACK #-} !Collection {-# UNPACK #-} !UpdateFlags !Selector !Update
        		   | Insert {-# UNPACK #-} !Database {-# UNPACK #-} !Collection {-# UNPACK #-} !InsertFlags !(Vector Document)
        		   | Query
        		   | GetMore {-# UNPACK #-} !Database {-# UNPACK #-} !Collection {-# UNPACK #-} !Word32 {-# UNPACK #-} CursorId
        		   | Delete {-# UNPACK #-} !Database {-# UNPACK #-} !Collection {-# UNPACK #-} !DeleteFlags !Selector
        		   | KillCursors {-# UNPACK #-} !(UnboxedVector CursorId)
