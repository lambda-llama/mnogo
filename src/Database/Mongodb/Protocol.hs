{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Database.Mongodb.Protocol
    ( Request(..)
    , OpCode(..)
    , RequestId
    , MessageHeader(..)
    , Reply
    , ReplyId
    ) where

#include "protocol.h"

import Control.Monad (unless)
import Data.Int (Int32, Int64)
import Data.Monoid ((<>))

import Data.Binary (Binary(..))
import Data.Binary.Put (Put, putWord32le, putWord64le)
import Data.Binary.Get (Get, getWord32le, getWord64le)
import Data.BitSet.Generic (BitSet(..))
import Data.Bson (Document)
import Data.Bson.Binary (putCString, putDocument, getDocument)
import Data.Tagged (Tagged)
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

newtype OpCode = OpCode { unOpCode :: Int32 }
    deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

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

class Request rq where
    type ResponseExpected rq :: Bool
    opCode :: Tagged rq OpCode
    putRequest :: rq -> Put

data Update = Update !FullCollection !UpdateFlags !Selector !UpdateSpec
    deriving (Eq, Show)

data Insert = Insert !FullCollection !InsertFlags !(Vector Document)
    deriving (Eq, Show)

data Query = Query !FullCollection !QueryFlags !Skip !Return !Selector
    deriving (Eq, Show)

data GetMore = GetMore !FullCollection !Return !CursorId
    deriving (Eq, Show)

data Delete = Delete !FullCollection !DeleteFlags !Selector
    deriving (Eq, Show)

data KillCursors = KillCursors !(UnboxedVector CursorId)
    deriving (Eq, Show)

instance Request Update where
    type ResponseExpected Update = False
    opCode = OP_UPDATE
    putRequest (Update c f s u ) = do
        putCString $ unFullCollection c
        putInt32 $ getBits f
        putDocument $ unSelector s
        putDocument $ unUpdateSpec u
    {-# INLINE putRequest #-}

instance Request Insert where
    type ResponseExpected Insert = False
    opCode = OP_INSERT
    putRequest (Insert c f ds) = do
        putCString $ unFullCollection c
        putInt32 $ getBits f
        Vector.forM_ ds putDocument
    {-# INLINE putRequest #-}

instance Request Query where
    type ResponseExpected Query = True
    opCode = OP_QUERY
    putRequest (Query c f s r d) = do
        putCString $ unFullCollection c
        putInt32 $ getBits f
        putInt32 $ unSkip s
        putInt32 $ unReturn r
        putDocument $ unSelector d
    {-# INLINE putRequest #-}

instance Request GetMore where
    type ResponseExpected GetMore = True
    opCode = OP_GET_MORE
    putRequest (GetMore c r i) = do
        putInt32 0
        putCString $ unFullCollection c
        putInt32 $ unReturn r
        putInt64 $ unCursorId i
    {-# INLINE putRequest #-}

instance Request Delete where
    type ResponseExpected Delete = False
    opCode = OP_DELETE
    putRequest (Delete c f s) = do
        putInt32 0
        putCString $ unFullCollection c
        putInt32 $ getBits f
        putDocument $ unSelector s
    {-# INLINE putRequest #-}

instance Request KillCursors where
    type ResponseExpected KillCursors = False
    opCode = OP_KILL_CURSORS
    putRequest (KillCursors is) = do
        putInt32 0
        putInt32 $ fromIntegral $ UnboxedVector.length is
        UnboxedVector.forM_ is (putInt64 . unCursorId)
    {-# INLINE putRequest #-}

type RequestId = Int32

data MessageHeader = MessageHeader
    { rhMessageLength :: !Int32
    , rhRequestId     :: !RequestId
    , rhResponseTo    :: !Int32
    , rhOpCode        :: !OpCode
    }

instance Binary MessageHeader where
    get = getMessageHeader
    put = putMessageHeader

data Reply = Reply !ReplyFlags !CursorId !Skip !Return !(Vector Document)
    deriving (Eq, Show)

instance Binary Reply where
    get = getReply
    put = undefined

type ReplyId = Int32

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

getMessageHeader :: Get MessageHeader
getMessageHeader = do
    rhMessageLength <- getInt32
    rhRequestId <- getInt32
    rhResponseTo <- getInt32
    rhOpCode <- fmap OpCode getInt32
    unless (rhOpCode == OP_REPLY) $
        fail $ "getReplyMessage: Expected OP_REPLY, got " <> show rhOpCode
    return $ MessageHeader { .. }
{-# INLINE getMessageHeader #-}

putMessageHeader :: MessageHeader -> Put
putMessageHeader (MessageHeader { .. }) = do
    putInt32 rhMessageLength
    putInt32 rhRequestId
    putInt32 rhResponseTo
    putInt32 $ unOpCode rhOpCode
{-# INLINE putMessageHeader #-}

getReply :: Get Reply
getReply = do
    f  <- fmap BitSet $ getInt32
    i  <- fmap CursorId $ getInt64
    s  <- fmap Skip getInt32
    r  <- fmap Return getInt32
    ds <- Vector.replicateM (fromIntegral r) getDocument
    return $ Reply f i s r ds
{-# INLINE getReply #-}
