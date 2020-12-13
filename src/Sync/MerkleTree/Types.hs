module Sync.MerkleTree.Types where

import System.FilePath
import Data.Int
import Crypto.Hash
import Data.Ord
import GHC.Generics
import Data.Text (Text, unpack)
import Sync.MerkleTree.Trie
import Data.Bytes.Serial (Serial, serialize)
import Data.Bytes.Put (runPutS)

-- | Information about a file that we expect to change, when the contents change.
data File
    = File
      { f_name :: Path
      , f_size :: FileSize
      , f_modtime :: FileModTime
      }
      deriving (Eq, Ord, Generic, Serial)

data Entry
    = FileEntry File
    | DirectoryEntry Path
    deriving (Eq, Generic, Serial)

newtype FileSize = FileSize { unFileSize :: Int64 }
    deriving (Eq, Ord, Generic)
    deriving newtype Num
    deriving anyclass Serial

data FileModTime = FileModTime { unModTime :: !Int64 }
    deriving (Eq, Ord, Generic)
    deriving anyclass Serial

-- | Representation for paths below the synchronization root directory
data Path
    = Root
    | Path Text Path
    deriving (Eq, Ord, Generic, Serial)

-- | Returns the string representation of a path
toFilePath :: FilePath -> Path -> FilePath
toFilePath fp p =
    case p of
      Root -> fp
      Path x y -> (toFilePath fp y) </> (unpack x)

-- | Entries are sorted first according to their depth in the path which is useful for directory
-- operations
instance Ord Entry where
    compare = comparing withLevel
        where
          withLevel entry = (levelOf entry, toEither entry)
          toEither entry =
              case entry of
                DirectoryEntry p -> Right p
                FileEntry f -> Left f

-- | Return the depth of an entries path
levelOf :: Entry -> Int
levelOf e =
    case e of
      DirectoryEntry p -> level p
      FileEntry f -> level $ f_name f
    where
      level Root = 0
      level (Path _ p) = 1 + level p

instance HasDigest Entry where
    digest = hash . runPutS . serialize
