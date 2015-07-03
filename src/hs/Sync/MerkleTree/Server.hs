module Sync.MerkleTree.Server where

import Data.ByteString(ByteString)
import System.IO.Streams(OutputStream)
import qualified Data.Serialize as SE
import Sync.MerkleTree.Util.RequestMonad
import Sync.MerkleTree.CommTypes
import qualified System.IO.Streams as ST
import Sync.MerkleTree.Trie
import qualified Data.Text.IO as T
import Sync.MerkleTree.Types
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.ByteString as BS
import System.IO

respond :: (Show a, SE.Serialize a) => OutputStream ByteString -> a -> IO ()
respond os = mapM_ (flip ST.write os . Just) . (:[BS.empty]) . SE.encode

runServer :: RunSide
runServer fp i o trie = loop (M.empty, 0)
    where
      addHandle (hs,next) h = withHandle next (Just h) (M.insert next h hs, next+1)
      withMsgHandle ch (handles, j) = withHandle ch (M.lookup ch handles) (handles, j)
      withHandle mh h handles =
          do let Just h' = h
             bs <- BS.hGet h' 4096
             case () of
               () | BS.null bs -> hClose h' >> respond o Final >> loop handles
                  | otherwise -> (respond o $ ToBeContinued bs $ ContinuationHandle mh) >> loop handles
      loop :: (Map Int Handle, Int) -> IO ()
      loop handles =
          do l' <- getFromInputStream i
             case l' of
               QuerySet l -> (respond o $ querySet trie l) >> loop handles
               QueryHash l -> (respond o $ queryHash trie l) >> loop handles
               Log msg -> T.putStrLn (unSerText msg) >> respond o True >> loop handles
               QueryFileContinuation (ContinuationHandle h) -> withMsgHandle h handles
               QueryFile f -> openFile (toFilePath fp f) ReadMode >>= addHandle handles
               Terminate -> respond o True >> return ()
