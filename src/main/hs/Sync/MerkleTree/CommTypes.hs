{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Sync.MerkleTree.CommTypes where

import GHC.Generics
import Data.Set(Set)
import Data.Serialize
import Data.ByteString(ByteString)
import Data.Typeable

import Sync.MerkleTree.Types
import Sync.MerkleTree.Trie

class Monad m => Protocol m where
    queryHashReq :: TrieLocation -> m Fingerprint
    querySetReq :: TrieLocation -> m (Set Entry)
    logReq :: SerText -> m Bool
    queryFileReq :: Path -> m QueryFileResponse
    queryFileContReq :: ContHandle -> m QueryFileResponse
    terminateReq :: m Bool

data ContHandle = ContHandle Int
    deriving (Show, Generic, Typeable)

instance Serialize ContHandle

data ClientServerOptions
    = ClientServerOptions
      { cs_add :: Bool
      , cs_update :: Bool
      , cs_delete :: Bool
      , cs_ignore :: [FilePath]
      }
      deriving (Read, Show)

data Request
    = QuerySet TrieLocation
    | QueryHash TrieLocation
    | Log SerText
    | QueryFile Path
    | QueryFileCont ContHandle
    | Terminate
    deriving (Generic, Show, Typeable)

instance Serialize Request

data QueryFileResponse
    = Final
    | ToBeContinued ByteString ContHandle
    deriving (Generic, Show, Typeable)

instance Serialize QueryFileResponse

data ProtocolVersion
    = Version1
    deriving (Read, Show, Eq)

thisProtocolVersion :: ProtocolVersion
thisProtocolVersion = Version1

data LaunchMessage
    = LaunchMessage
    { lm_protocolVersion :: ProtocolVersion
    , lm_dir :: FilePath
    , lm_side :: Side
    , lm_clientServerOptions :: ClientServerOptions
    }
    deriving (Read, Show)

data Side = Server | Client
    deriving (Read, Show)
