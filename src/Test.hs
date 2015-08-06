import qualified Sync.MerkleTree.Test as T
import Control.Monad
import qualified Test.HUnit as H

main =
    do let allTests = H.TestList $ [T.tests]
       putStrLn "Running tests:"
       forM (H.testCasePaths allTests) $ putStrLn . ("   " ++) . show
       counts <- H.runTestTT allTests
       unless (H.errors counts == 0 && H.failures counts == 0) $ fail $ show counts
