{-# OPTIONS_GHC -funbox-strict-fields #-}

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

newtype Selector = Selector { unSelector :: Document }

newtype Skip = Skip { unSkip :: Word32 }
newtype Return = Return { unReturn :: Word32 }

newtype FullCollection = FullCollection { unFullCollection :: Text }
  deriving (Eq, Ord, Show)

newtype CursorId = CursorId { unCursorId :: Word64 }
  deriving (Eq, Ord, Show, GenericBaseVector UnboxedVector,
            GenericMutableVector UnboxedMutableVector, Unbox)

data UpdateFlag = Upsert
                | MultiUpdate
  deriving (Eq, Ord, Show)

data InsertFlag = ContinueOnError
  deriving (Eq, Ord, Show)

data QueryFlag = TailableCursor
               | SlaveOk
               | OplogReplay
               | NoCursorTimeout
               | AwaitData
               | Exhaust
               | Partial

data DeleteFlag = SingleRemove
  deriving (Eq, Ord, Show)

type UpdateFlags = BitSet UpdateFlag
type InsertFlags = BitSet InsertFlag
type QueryFlags = BitSet QueryFlags
type DeleteFlags = BitSet DeleteFlag

data Request = Update !FullCollection !UpdateFlags !Selector !Update
             | Insert !FullCollection !InsertFlags !(Vector Document)
             | Query !FullCollection !QueryFlags !Skip !Return !Selector
             | GetMore !FullCollection !Word32 !CursorId
             | Delete !FullCollection !DeleteFlags !Selector
             | KillCursors !(UnboxedVector CursorId)
  deriving (Eq, Ord, Show)

data Reply = Reply !ReplyFlags !CursorId !Skip !Return !(Vector Document)
  deriving (Eq, Ord, Show)
