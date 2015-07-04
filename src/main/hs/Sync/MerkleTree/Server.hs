{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Sync.MerkleTree.Server where

import Control.Monad.State
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Trie
import qualified Data.Text.IO as T
import Sync.MerkleTree.Types
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.ByteString as BS
import System.IO

data ServerState
    = ServerState
    { st_handles :: Map Int Handle
    , st_nextHandle :: Int
    , st_trie :: Trie Entry
    , st_path :: FilePath
    }

type ServerMonad = StateT ServerState IO

startServerState :: FilePath -> Trie Entry -> ServerState
startServerState fp trie =
    ServerState
    { st_handles = M.empty
    , st_nextHandle = 0
    , st_trie = trie
    , st_path = fp
    }

instance ProtocolM ServerMonad where
    querySetReq l = get >>= (\s -> return $ querySet (st_trie s) l)
    queryHashReq l = get >>= (\s -> return $ queryHash (st_trie s) l)
    logReq (SerText msg) = liftIO (T.hPutStrLn stderr msg) >> return True
    queryFileContReq (ContHandle n) =
         do s <- get
            let Just h = M.lookup n (st_handles s)
            withHandle h n
    queryFileReq f =
          do s <- get
             h <- liftIO $ openFile (toFilePath (st_path s) f) ReadMode
             let n = st_nextHandle s
             put $ s { st_handles = M.insert n h (st_handles s), st_nextHandle = n + 1 }
             withHandle h n
    terminateReq = return True

withHandle :: Handle -> Int -> ServerMonad QueryFileResponse
withHandle h n =
    do bs <- liftIO $ BS.hGet h (2^17)
       case () of
         () | BS.null bs ->
             do liftIO $ hClose h
                modify (\s -> s { st_handles = M.delete n (st_handles s) })
                return $ Final
            | otherwise -> return $ ToBeContinued bs $ ContHandle n
