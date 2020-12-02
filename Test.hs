import Control.Monad
import qualified Sync.MerkleTree.Sync as S
import qualified Sync.MerkleTree.Test as T
import qualified Sync.MerkleTree.Trie as TR
import qualified Test.HUnit as H

main :: IO ()
main =
    do let allTests = H.TestList $ [T.tests, TR.tests, S.tests]
       putStrLn "Running tests:"
       forM (H.testCasePaths allTests) $ putStrLn . ("   " ++) . show
       counts <- H.runTestTT allTests
       unless (H.errors counts == 0 && H.failures counts == 0) $ fail "Tests failed."
