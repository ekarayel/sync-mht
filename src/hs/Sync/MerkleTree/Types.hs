{-# LANGUAGE DeriveGeneric #-}
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
      deriving (Show, Eq, Ord, Generic)

instance SE.Serialize File

data Entry
    = FileEntry File
    | DirectoryEntry Path
    deriving (Show, Eq, Generic)

instance SE.Serialize Entry

data FileSize = FileSize { unFileSize :: !Int64 }
    deriving (Show, Eq, Ord, Generic)

instance SE.Serialize FileSize
instance Num FileSize where
   (+) (FileSize x) (FileSize y) = FileSize $ x + y
   (*) (FileSize x) (FileSize y) = FileSize $ x * y
   negate (FileSize x) = FileSize $ negate x
   abs (FileSize x) = FileSize $ abs x
   signum (FileSize x) = FileSize $ signum x
   fromInteger = FileSize . fromInteger

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

