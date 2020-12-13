module Sync.MerkleTree.Util.RPCClientSpec (spec) where

import Control.Applicative (liftA2)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan)
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Sync.MerkleTree.Util.Communication
import Sync.MerkleTree.Util.RPCClient
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Concurrent (chanToInput, chanToOutput)
import Test.Hspec

mkChanStreams :: IO (InputStream ByteString, OutputStream ByteString)
mkChanStreams = do
  chan <- newChan
  liftM2 (,) (chanToInput chan) (chanToOutput chan)

runWithServer :: RPCClient a -> IO a
runWithServer f = do
  (serverInStream, clientOutStream) <- mkChanStreams
  (clientInStream, serverOutStream) <- mkChanStreams
  let serverLoop = do
        x <- receive serverInStream
        send serverOutStream $ 'r' : x
        if null x
          then return ()
          else serverLoop
  _ <- forkIO $ serverLoop
  runRPCClient clientInStream clientOutStream f

testCall :: (HasCall m) => String -> m String
testCall = call

spec :: Spec
spec =
  describe "RPCClient" $ do
    it "orders calls correctly" $
      runWithServer $ do
        r <- liftA2 (,) (testCall "a") (testCall "b")
        liftIO $ r `shouldBe` ("ra", "rb")
        s <- liftM2 (,) (testCall "c") (testCall "d")
        liftIO $ s `shouldBe` ("rc", "rd")
        e <- testCall ""
        liftIO $ e `shouldBe` "r"
    it "does not call when non necessary" $ runRPCClient undefined undefined $ return ()
    it "forwards fails" $ (runRPCClient undefined undefined $ fail "") `shouldThrow` anyException
