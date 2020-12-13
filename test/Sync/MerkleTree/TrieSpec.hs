module Sync.MerkleTree.TrieSpec (spec) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sync.MerkleTree.Trie (TrieLocation (..), tl_index, tl_level)
import qualified Sync.MerkleTree.Trie as T
import Test.Hspec

newtype TestDigest = TestDigest {unTestDigest :: T.Text}
  deriving (Eq, Ord, Show)

instance T.HasDigest TestDigest where
  digest = T.hashMD5 . TE.encodeUtf8 . unTestDigest

testTrie :: T.Trie TestDigest
testTrie = T.mkTrie 0 $ map (TestDigest . T.pack . show) [0 .. 168]

spec :: Spec
spec = do
  describe "lookup" $ do
    it "fails when level < 0" $ do
      (T.lookup testTrie $ TrieLocation {tl_level = -1, tl_index = 0}) `shouldBe` Nothing
    it "fails when level too high" $ do
      (T.lookup testTrie $ TrieLocation {tl_level = 4, tl_index = 0}) `shouldBe` Nothing

-- TODO Add useful tests for Trie.
