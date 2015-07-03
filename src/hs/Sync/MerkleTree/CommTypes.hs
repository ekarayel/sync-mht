{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Sync.MerkleTree.CommTypes where

import GHC.Generics
import qualified Data.Serialize as SE
import Data.ByteString(ByteString)
import Data.Typeable
import System.IO.Streams(InputStream, OutputStream)

import Sync.MerkleTree.Types
import Sync.MerkleTree.Trie

type RunSide = FilePath -> InputStream ByteString -> OutputStream ByteString -> Trie Entry -> IO ()

data ContinuationHandle = ContinuationHandle Int
    deriving (Show, Generic, Typeable)

data Request
    = QuerySet TrieLocation
    | QueryHash TrieLocation
    | Log SerText
    | QueryFile Path
    | QueryFileContinuation ContinuationHandle
    | Terminate
    deriving (Generic, Show, Typeable)

data QueryFileResponse
    = Final
    | ToBeContinued ByteString ContinuationHandle
    deriving (Generic, Show, Typeable)

instance SE.Serialize ContinuationHandle
instance SE.Serialize Request
instance SE.Serialize QueryFileResponse