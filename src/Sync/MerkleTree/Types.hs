{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sync.MerkleTree.Types where

import System.FilePath
import Data.Int
import Crypto.Hash
import Data.Ord
import GHC.Generics
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
      deriving (Eq, Ord, Generic)

instance SE.Serialize File

data Entry
    = FileEntry File
    | DirectoryEntry Path
    deriving (Eq, Generic)

instance SE.Serialize Entry

newtype FileSize = FileSize { unFileSize :: Int64 }
    deriving (Eq, Ord, Generic, Num)

instance SE.Serialize FileSize

data FileModTime = FileModTime { unModTime :: !Int64 }
    deriving (Eq, Ord, Generic)

instance SE.Serialize FileModTime

data Path
    = Root
    | Path SerText Path
    deriving (Eq, Ord, Generic)

toFilePath :: FilePath -> Path -> FilePath
toFilePath fp p =
    case p of
      Root -> fp
      Path x y -> (toFilePath fp y) </> (T.unpack $ unSerText x)

instance SE.Serialize Path

instance Ord Entry where
    compare = comparing withLevel
        where
          withLevel entry = (levelOf entry, toEither entry)
          toEither entry =
              case entry of
                DirectoryEntry p -> Right p
                FileEntry f -> Left f

levelOf :: Entry -> Int
levelOf e =
    case e of
      DirectoryEntry p -> level p
      FileEntry f -> level $ f_name f
    where
      level Root = 0
      level (Path _ p) = 1 + level p

instance HasDigest Entry where
    digest = hash . SE.encode

data SerText = SerText { unSerText :: !T.Text }
    deriving (Ord, Eq)

instance SE.Serialize SerText where
    get = SE.get >>= either (fail . show) (return . SerText) . TE.decodeUtf8'
    put = SE.put . TE.encodeUtf8 . unSerText
