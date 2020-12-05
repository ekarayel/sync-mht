 {-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.SyncSpec (spec) where

import Control.Concurrent.MVar
import Test.Hspec
import qualified Data.Bytes.Serial as SE
import qualified Data.Bytes.Put as P
import qualified System.IO.Streams as ST
import qualified System.IO.Streams.Concurrent as ST

import Sync.MerkleTree.Sync
import Sync.MerkleTree.Analyse
import Sync.MerkleTree.Channel
import Sync.MerkleTree.Client
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Server
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import Sync.MerkleTree.Util.GetFromInputStream

spec :: Spec
spec = do
  describe "sync-mht" $ do
    it "checks protocol version" $ do
      inst <- ST.fromByteString $ P.runPutS $ SE.serialize $ show $
        LaunchMessage
        { lm_protocolVersion = ProtocolVersion 1
        , lm_dir = "/"
        , lm_side = Client
        , lm_clientServerOptions =
            ClientServerOptions
            { cs_add = False
            , cs_update = False
            , cs_delete = False
            , cs_ignore = []
            , cs_compareClocks = Nothing
            }
        }
      out <- ST.nullOutput
      r <- newEmptyMVar
      (child r $ StreamPair { sp_in = inst, sp_out = out })
              `shouldThrow` anyException

