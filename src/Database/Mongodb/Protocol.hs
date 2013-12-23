{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Mongodb.Protocol () where

import Data.Int (Int32, Int64)

import Data.BitSet (BitSet)
import Data.Bson (Document, )
import Data.Text (Text)
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
  deriving (Eq, Show)

newtype UpdateSpec = UpdateSpec { unUpdateSpec :: Document }
  deriving (Eq, Show)

newtype Skip = Skip { unSkip :: Int32 }
  deriving (Eq, Show)

newtype Return = Return { unReturn :: Int32 }
  deriving (Eq, Show)

newtype FullCollection = FullCollection { unFullCollection :: Text }
  deriving (Eq, Show)

newtype CursorId = CursorId { unCursorId :: Int64 }
  deriving (Eq, Show, GenericBaseVector UnboxedVector,
            GenericMutableVector UnboxedMutableVector, Unbox)

data UpdateFlag = Upsert
                | MultiUpdate
  deriving (Eq, Show, Enum)

data InsertFlag = ContinueOnError
  deriving (Eq, Show, Enum)

data QueryFlag = TailableCursor
               | SlaveOk
               | OplogReplay
               | NoCursorTimeout
               | AwaitData
               | Exhaust
               | Partial
  deriving (Eq, Show, Enum)

data DeleteFlag = SingleRemove
  deriving (Eq, Show, Enum)

data ReplyFlag = CursorNotFound
               | QueryFailure
               | ShardConfigStale
               | AwaitCapable
  deriving (Eq, Show, Enum)

type UpdateFlags = BitSet UpdateFlag
type InsertFlags = BitSet InsertFlag
type QueryFlags = BitSet QueryFlag
type DeleteFlags = BitSet DeleteFlag
type ReplyFlags = BitSet ReplyFlag

data Request = Update !FullCollection !UpdateFlags !Selector !UpdateSpec
             | Insert !FullCollection !InsertFlags !(Vector Document)
             | Query !FullCollection !QueryFlags !Skip !Return !Selector
             | GetMore !FullCollection !Int32 !CursorId
             | Delete !FullCollection !DeleteFlags !Selector
             | KillCursors !(UnboxedVector CursorId)
  deriving (Eq, Show)

data Reply = Reply !ReplyFlags !CursorId !Skip !Return !(Vector Document)
  deriving (Eq, Show)
