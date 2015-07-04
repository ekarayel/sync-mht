{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Sync.MerkleTree.Trie where

import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Typeable
import Data.Byteable
import Crypto.Hash
import qualified Data.Set as S
import Data.Set(Set)
import Data.Array.IArray
import Control.Arrow hiding (arr, loop)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import GHC.Generics
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS
import qualified Data.Serialize as SE

data Hash = Hash { unHash :: BS.ByteString }
    deriving (Eq, Ord, Generic)

instance Show Hash where
    showsPrec i x = showsPrec i (go x)
        where
          go :: Hash -> String
          go (Hash dig) = T.unpack $ TE.decodeUtf8 $ B16.encode dig

instance Read Hash where
    readsPrec i = map (first go) . readsPrec i
        where
          go :: String -> Hash
          go s = Hash . fst . B16.decode $ TE.encodeUtf8 $ T.pack s

instance SE.Serialize Hash

data Trie a
    = Trie
      { t_hash :: !Hash
      , t_node :: !(TrieNode a)
      }
      deriving (Eq, Read, Show)

data TrieNode a
    = Node !(Array Int (Trie a))
    | Leave !(Set a)
    deriving (Eq, Read, Show)

data NodeType = NodeType | LeaveType
    deriving (Eq, Read, Show, Generic)
instance SE.Serialize NodeType

data TrieLocation
    = TrieLocation
    { tl_level :: Int
    , tl_index :: Int
    }
    deriving (Read, Show, Generic)

instance SE.Serialize TrieLocation

degree :: Int
degree = 8

class HasDigest a where
    digest :: a -> Digest SHA256

data Fingerprint
    = Fingerprint
      { f_hash :: Hash
      , f_nodeType :: NodeType
      }
      deriving (Eq, Read, Show, Generic, Typeable)

instance SE.Serialize Fingerprint

toFingerprint :: Trie a -> Fingerprint
toFingerprint (Trie h node) = Fingerprint h nodeType
     where
       nodeType =
           case node of
             Node _ -> NodeType
             Leave _ -> LeaveType

mkTrie :: (Ord a, HasDigest a) => Int -> [a] -> Trie a
mkTrie i ls
    | length ls < degree = mkLeave ls
    | otherwise =
        mkNode $ fmap (mkTrie (i+1)) $ accumArray (flip (:)) [] (0,degree-1) $ map ((groupOf i) &&& id) ls

mkNode :: (Array Int (Trie a)) -> Trie a
mkNode arr =
    Trie
    { t_hash = combineHash $ map t_hash $ elems arr
    , t_node = Node arr
    }

hashSHA256 :: BS.ByteString -> Digest SHA256
hashSHA256 = hash

combineHash :: [Hash] -> Hash
combineHash = Hash . toBytes . hashSHA256 . BS.concat . map unHash

groupOf :: (HasDigest a) => Int -> a -> Int
groupOf i x = fromInteger $ toInteger $ (h0 `mod` (fromInteger $ toInteger degree))
     where
       Just (h0, _t) = BS.uncons $ toBytes $ h
       h :: Digest SHA256
       h = hash $ BS.concat [BS.pack [fromInteger $ toInteger i], toBytes $ digest x]

mkLeave :: (HasDigest a, Ord a) => [a] -> Trie a
mkLeave ls =
    Trie
    { t_hash = combineHash $ map (Hash . toBytes . digest) $ L.sort ls
    , t_node = Leave $ S.fromList ls
    }

lookup :: Trie a -> TrieLocation -> Trie a
lookup trie (TrieLocation { tl_level = l, tl_index = i })
    | l < 0 || i < 0 || i >= degree^l = error "illegal index pair"
    | l > 0, (g, i') <- i `quotRem` (degree ^ (l-1)), Node arr <- t_node trie =
        lookup (arr ! g) (TrieLocation { tl_level = (l - 1), tl_index =  i'})
    | l == 0 = trie
    | otherwise = error "index pair to deep"

queryHash :: Trie a -> TrieLocation -> Fingerprint
queryHash trie = toFingerprint . lookup trie

querySet :: (Ord a) => Trie a -> TrieLocation -> Set a
querySet trie = getAll . lookup trie

getAll :: (Ord a) => Trie a -> Set a
getAll (Trie _ node) =
    case node of
      Node arr -> S.unions $ map getAll $ elems arr
      Leave s -> s

rootLocation :: TrieLocation
rootLocation =
    TrieLocation
    { tl_level = 0
    , tl_index = 0
    }

expand :: TrieLocation -> (Array Int (Trie a)) -> [(TrieLocation, Trie a)]
expand loc arr = map go [0..(degree - 1)]
    where
      go i =
          ( TrieLocation
            { tl_level = tl_level loc + 1
            , tl_index = degree * tl_index loc + i }
          , arr ! i
          )
