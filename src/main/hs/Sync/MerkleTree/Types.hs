{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sync.MerkleTree.Types where

import System.FilePath
import Data.Int
import Crypto.Hash
import Data.Ord
import GHC.Generics
import Data.Typeable
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Sync.MerkleTree.Trie
import qualified Data.Serialize as SE

data File
    = File
      { f_name :: Path
      , f_size :: FileSize
      , f_modtime :: FileModTime
      }
      deriving (Show, Eq, Ord, Generic, Typeable)

instance SE.Serialize File

data Entry
    = FileEntry File
    | DirectoryEntry Path
    deriving (Show, Eq, Generic, Typeable)

instance SE.Serialize Entry

newtype FileSize = FileSize { unFileSize :: Int64 }
    deriving (Show, Eq, Ord, Generic, Num, Typeable)

instance SE.Serialize FileSize

data FileModTime = FileModTime { unModTime :: !Int64 }
    deriving (Show, Eq, Ord, Generic)

instance SE.Serialize FileModTime

data Path
    = Root
    | Path SerText Path
    deriving (Eq, Ord, Generic)

instance Show Path where
    show x = toFilePath "/" x

toFilePath :: FilePath -> Path -> FilePath
toFilePath fp p =
    case p of
      Root -> fp
      Path x y -> (toFilePath fp y) </> (T.unpack $ unSerText x)

instance SE.Serialize Path

instance Ord Entry where
    compare = comparing withLevel
        where
          level Root = 0 :: Int
          level (Path _ p) = 1 + level p
          withLevel entry =
              case entry of
                DirectoryEntry p -> (level p, Right p)
                FileEntry f -> (level $ f_name f, Left f)

instance HasDigest Entry where
    digest = hash . SE.encode

data SerText = SerText { unSerText :: !T.Text }
    deriving (Ord, Show, Eq)

instance SE.Serialize SerText where
    get = SE.get >>= either (fail . show) (return . SerText) . TE.decodeUtf8'
    put = SE.put . TE.encodeUtf8 . unSerText

