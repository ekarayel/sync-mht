{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
module Sync.MerkleTree.CommTypes where

import GHC.Generics
import Data.Set(Set)
import Data.Bytes.Serial
import Data.Time.Clock
import Data.ByteString(ByteString)
import qualified Data.Text as T

import Sync.MerkleTree.Types
import Sync.MerkleTree.Trie

class MonadFail m => Protocol m where
    queryHashReq :: TrieLocation -> m Fingerprint
    querySetReq :: TrieLocation -> m (Set Entry)
    logReq :: T.Text -> m Bool
    queryFileReq :: Path -> m QueryFileResponse
    queryFileContReq :: ContHandle -> m QueryFileResponse
    terminateReq :: Maybe T.Text -> m Bool
    queryTime :: m UTCTime

data ContHandle = ContHandle Int
    deriving (Generic)

instance Serial ContHandle

-- | Configuration shared by both the client and server.
data ClientServerOptions
    = ClientServerOptions
      { cs_add :: Bool
      , cs_update :: Bool
      , cs_delete :: Bool
      , cs_ignore :: [FilePath]
      , cs_compareClocks :: Maybe (Double, Double)
      }
      deriving (Read, Show)

shouldAdd :: (?clientServerOptions :: ClientServerOptions) => Bool
shouldAdd = cs_add ?clientServerOptions

shouldUpdate :: (?clientServerOptions :: ClientServerOptions) => Bool
shouldUpdate = cs_update ?clientServerOptions

shouldDelete :: (?clientServerOptions :: ClientServerOptions) => Bool
shouldDelete = cs_delete ?clientServerOptions

ignorePaths :: (?clientServerOptions :: ClientServerOptions) => [FilePath]
ignorePaths = cs_ignore ?clientServerOptions

compareClocks :: (?clientServerOptions :: ClientServerOptions) => Maybe (Double, Double)
compareClocks = cs_compareClocks ?clientServerOptions

data Request
    = QuerySet TrieLocation
    | QueryHash TrieLocation
    | Log T.Text
    | QueryFile Path
    | QueryFileCont ContHandle
    | Terminate (Maybe T.Text)
    | QueryTime
    deriving (Generic)

instance Serial Request

data QueryFileResponse
    = Final
    | ToBeContinued ByteString ContHandle
    deriving (Generic)

instance Serial QueryFileResponse

data ProtocolVersion = ProtocolVersion !Int
    deriving (Read, Show, Eq)

thisProtocolVersion :: ProtocolVersion
thisProtocolVersion = ProtocolVersion 5

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
