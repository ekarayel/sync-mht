{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Sync.MerkleTree.Server where

import Codec.Compression.GZip
import Control.Monad.State
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Trie
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Sync.MerkleTree.Types
import qualified Data.Map as M
import Data.Map(Map)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO
import Data.Time.Clock

data ServerState
    = ServerState
    { st_handles :: Map Int Handle -- ^ Map of open file handles with their ids
    , st_nextHandle :: Int -- ^ Next available id
    , st_trie :: Trie Entry -- ^ Merkle Hash Tree of server file hierarchy
    , st_path :: FilePath -- ^ path of the root of the file hierarchy
    }

type ServerMonad = StateT ServerState IO

startServerState :: FilePath -> Trie Entry -> IO ServerState
startServerState fp trie =
    do T.hPutStr stderr $ T.pack $
           concat [ "Hash of source directory:      ", show $ t_hash trie, "\n" ]
       return $
           ServerState
           { st_handles = M.empty
           , st_nextHandle = 0
           , st_trie = trie
           , st_path = fp
           }

instance Protocol ServerMonad where
    querySetReq l = get >>= (\s -> querySet (st_trie s) l)
    queryHashReq l = get >>= (\s -> queryHash (st_trie s) l)
    logReq msg = liftIO (T.hPutStr stderr msg) >> return True
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
    queryTime = liftIO getCurrentTime
    terminateReq = return True

-- | Respond to a queryFile or queryFileCont request for a given file handle and id
withHandle :: Handle -> Int -> ServerMonad QueryFileResponse
withHandle h n =
    do bs <- liftIO $ BS.hGet h (2^(17::Int))
       case () of
         () | BS.null bs ->
             do liftIO $ hClose h
                modify (\s -> s { st_handles = M.delete n (st_handles s) })
                return $ Final
            | otherwise ->
                return $ ToBeContinued (BL.toStrict $ compress $ BL.fromStrict bs) $ ContHandle n
