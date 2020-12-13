module Sync.MerkleTree.SyncSpec (spec) where

import Control.Concurrent.MVar
import qualified Data.Bytes.Put as P
import qualified Data.Bytes.Serial as SE
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Sync
import qualified System.IO.Streams as ST
import Test.Hspec

spec :: Spec
spec = do
  describe "sync-mht" $ do
    it "checks protocol version" $ do
      inst <-
        ST.fromByteString $
          P.runPutS $
            SE.serialize $
              show $
                LaunchMessage
                  { lm_protocolVersion = ProtocolVersion 1,
                    lm_dir = "/",
                    lm_side = Client,
                    lm_clientServerOptions =
                      ClientServerOptions
                        { cs_add = False,
                          cs_update = False,
                          cs_delete = False,
                          cs_ignore = [],
                          cs_compareClocks = Nothing
                        }
                  }
      out <- ST.nullOutput
      r <- newEmptyMVar
      (child r $ StreamPair {sp_in = inst, sp_out = out})
        `shouldThrow` anyException
