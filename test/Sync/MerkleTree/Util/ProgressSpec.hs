module Sync.MerkleTree.Util.ProgressSpec (spec) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid
import Sync.MerkleTree.Util.Progress
import Test.Hspec

runTestProgress :: ((ProgressState (Sum Int) -> ProgressMonad (Sum Int) IO ()) -> ProgressMonad (Sum Int) IO ()) -> IO ()
runTestProgress op =
  do
    ref <- newIORef $ ProgressState (Sum 0) (Sum 0)
    let checker :: ProgressState (Sum Int) -> ProgressMonad (Sum Int) IO ()
        checker x = liftIO $ do
          actual <- readIORef ref
          actual `shouldBe` x
    runProgress (writeIORef ref) (op checker)

spec :: Spec
spec =
  describe "progress" $ do
    it "reports planned and completed work" $
      runTestProgress $ \check -> do
        check (ProgressState (Sum 3) (Sum 0))
          *> progress (Sum (1 :: Int))
          *> check (ProgressState (Sum 3) (Sum 1))
          *> progress (Sum (2 :: Int))
        check (ProgressState (Sum 3) (Sum 3))
        progress (Sum (2 :: Int))
        check (ProgressState (Sum 5) (Sum 5))
    it "supports spliting planning and completion" $
      runTestProgress $ \check -> do
        check (ProgressState (Sum 0) (Sum 0))
        progressPlanned (Sum (2 :: Int))
        check (ProgressState (Sum 2) (Sum 0))
        progressCompleted (Sum (2 :: Int))
        check (ProgressState (Sum 2) (Sum 2))
