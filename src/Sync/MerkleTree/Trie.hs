{-# LANGUAGE DeriveGeneric #-}
module Sync.MerkleTree.Trie where

import Prelude hiding (lookup)
import Control.Monad
import Control.Arrow hiding (arr, loop)
import Crypto.Hash
import Data.Array.IArray
import Data.Byteable
import Data.Set(Set)
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.List as L
import qualified Data.Serialize as SE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Test.HUnit as H

data Hash = Hash { unHash :: !BS.ByteString }
    deriving (Eq, Generic)

instance Show Hash where
    showsPrec _ x = ((T.unpack $ TE.decodeUtf8 $ B16.encode $ unHash x) ++)

instance SE.Serialize Hash

-- Abstract Merkle Hash Trie
data Trie a
    = Trie
      { t_hash :: !Hash
      , t_node :: !(TrieNode a)
      }
      deriving (Show, Eq)

data TrieNode a
    = Node !(Array Int (Trie a))
    | Leave !(Set a)
    deriving (Show, Eq)

data NodeType = NodeType | LeaveType
    deriving (Eq, Generic)
instance SE.Serialize NodeType

-- Location in the Merkle Hash Trie
data TrieLocation
    = TrieLocation
    { tl_level :: !Int -- ^ Must be nonnegative
    , tl_index :: !Int -- ^ Must be between nonnegative and smaller than (degree^tl_level)
    }
    deriving (Generic)

instance SE.Serialize TrieLocation

degree :: Int
degree = 64

class HasDigest a where
    digest :: a -> Digest SHA256

-- | Fingerprint of a Merkle-Hash-Tree node
-- We asssume the Tree below a node is identical while synchronizing if its FingerPrint is
data Fingerprint
    = Fingerprint
      { f_hash :: !Hash
      , f_nodeType :: !NodeType
      }
      deriving (Eq, Generic)

instance SE.Serialize Fingerprint

toFingerprint :: Trie a -> Fingerprint
toFingerprint (Trie h node) = Fingerprint h nodeType
     where
       nodeType =
           case node of
             Node _ -> NodeType
             Leave _ -> LeaveType

-- | Creates a Merkle-Hash-Tree for a list of elements
mkTrie :: (Ord a, HasDigest a) => Int -> [a] -> Trie a
mkTrie i ls
    | length ls < degree = mkLeave ls
    | otherwise =
        mkNode
        $ fmap (mkTrie (i+1))
        $ accumArray (flip (:)) [] (0,degree-1)
        $ map ((groupOf i) &&& id) ls

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

-- | The function @groupOf x@ eeturns a value between 0 to degree-1 for a digest with the property
-- that @groupOf@ forms an approximate unviversal hash familiy.
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

lookup :: (Monad m) => Trie a -> TrieLocation -> m (Trie a)
lookup trie (TrieLocation { tl_level = l, tl_index = i })
    | l < 0 || i < 0 || i >= degree^l = fail "illegal index pair"
    | l > 0, (g, i') <- i `quotRem` (degree ^ (l-1)), Node arr <- t_node trie =
        lookup (arr ! g) (TrieLocation { tl_level = (l - 1), tl_index =  i'})
    | l == 0 = return trie
    | otherwise = fail "index pair to deep"

queryHash :: (Monad m) => Trie a -> TrieLocation -> m Fingerprint
queryHash trie = liftM toFingerprint . lookup trie

querySet :: (Ord a, Monad m) => Trie a -> TrieLocation -> m (Set a)
querySet trie = liftM getAll . lookup trie

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

newtype TestDigest = TestDigest { unTestDigest :: T.Text }
    deriving (Eq, Ord, Show)

instance HasDigest TestDigest where
    digest = hashSHA256 . TE.encodeUtf8 . unTestDigest

tests :: H.Test
tests = H.TestList $
    [ H.TestLabel "trieLookup"
        $ (Nothing H.~=? (lookup t (TrieLocation { tl_level = -1, tl_index = 0 })))
    , H.TestLabel "trieLookupTooDeep"
        $ (Nothing H.~=? (lookup t (TrieLocation { tl_level = 4, tl_index = 0 })))
    ]
    where
      t = mkTrie 0 $ map (TestDigest . T.pack . show) [0..(13+2*degree*degree)]
