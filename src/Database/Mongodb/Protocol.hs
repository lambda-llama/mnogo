{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Database.Mongodb.Protocol () where

#include "protocol.h"

import Data.Int (Int32, Int64)

import Data.Binary.Put (Put, putWord32le, putWord64le)
import Data.Binary.Get (Get, getWord32le, getWord64le)
import Data.BitSet.Generic (BitSet(..))
import Data.Bson (Document)
import Data.Bson.Binary (putCString, putDocument, getDocument)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector as Vector
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
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

newtype Return = Return { unReturn :: Int32 }
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

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

type UpdateFlags = BitSet Int32 UpdateFlag
type InsertFlags = BitSet Int32 InsertFlag
type QueryFlags = BitSet Int32 QueryFlag
type DeleteFlags = BitSet Int32 DeleteFlag
type ReplyFlags = BitSet Int32 ReplyFlag

data Request = Update !FullCollection !UpdateFlags !Selector !UpdateSpec
             | Insert !FullCollection !InsertFlags !(Vector Document)
             | Query !FullCollection !QueryFlags !Skip !Return !Selector
             | GetMore !FullCollection !Return !CursorId
             | Delete !FullCollection !DeleteFlags !Selector
             | KillCursors !(UnboxedVector CursorId)
  deriving (Eq, Show)

data Reply = Reply !ReplyFlags !CursorId !Skip !Return !(Vector Document)
  deriving (Eq, Show)

getInt32 :: Get Int32
getInt32 = fmap fromIntegral getWord32le
{-# INLINE getInt32 #-}

getInt64 :: Get Int64
getInt64 = fmap fromIntegral getWord64le
{-# INLINE getInt64 #-}

putInt32 :: Int32 -> Put
putInt32 = putWord32le . fromIntegral
{-# INLINE putInt32 #-}

putInt64 :: Int64 -> Put
putInt64 = putWord64le . fromIntegral
{-# INLINE putInt64 #-}

putRequest :: Request -> Put
putRequest (Update c f s u ) = do
  putInt32 OP_UPDATE
  putCString $ unFullCollection c
  putInt32 $ getBits f
  putDocument $ unSelector s
  putDocument $ unUpdateSpec u
putRequest (Insert c f ds) = do
  putInt32 OP_UPDATE
  putCString $ unFullCollection c
  putInt32 $ getBits f
  Vector.forM_ ds putDocument
putRequest (Query c f s r d) = do
  putInt32 OP_QUERY
  putCString $ unFullCollection c
  putInt32 $ getBits f
  putInt32 $ unSkip s
  putInt32 $ unReturn r
  putDocument $ unSelector d
putRequest (GetMore c r i) = do
  putInt32 OP_GET_MORE
  putInt32 0
  putCString $ unFullCollection c
  putInt32 $ unReturn r
  putInt64 $ unCursorId i
putRequest (Delete c f s) = do
  putInt32 OP_DELETE
  putInt32 0
  putCString $ unFullCollection c
  putInt32 $ getBits f
  putDocument $ unSelector s
putRequest (KillCursors is) = do
  putInt32 OP_KILL_CURSORS
  putInt32 0
  putInt32 $ fromIntegral $ UnboxedVector.length is
  UnboxedVector.forM_ is (putInt64 . unCursorId)
{-# INLINE putRequest #-}

getReply :: Get Reply
getReply = do
  f <- fmap BitSet $ getInt32
  i <- fmap CursorId $ getInt64
  s <- fmap Skip getInt32
  r <- fmap Return getInt32
  ds <- Vector.replicateM (fromIntegral r) getDocument
  return $! Reply f i s r ds
{-# INLINE getReply #-}
