{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Sync
    ( child
    , local
    , parent
    , openStreams
    , Direction(..)
    ) where

import Control.Monad
import Control.Monad.State
import Data.Monoid
import System.FilePath
import Foreign.C.Types
import Prelude hiding (lookup)
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import System.Directory
import System.IO
import System.IO.Streams(InputStream, OutputStream)
import System.Posix.Types
import System.Posix.Files
import Data.ByteString(ByteString)
import qualified Data.Serialize as SE
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified System.IO.Streams as ST

import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Client
import Sync.MerkleTree.Server
import Sync.MerkleTree.Util.RequestMonad

isRealFile :: String -> Bool
isRealFile x
    | x `elem` [".", ".."] = False
    | otherwise = True

analyseDirectory :: FilePath -> [FilePath] -> Path -> IO [Entry]
analyseDirectory fp ignore path
    | fp `elem` ignore = return []
    | otherwise =
        do files <- getDirectoryContents fp
           liftM concat $ mapM (analyse fp ignore path) $ filter isRealFile files

analyse :: FilePath -> [FilePath] -> Path -> String -> IO [Entry]
analyse fp ignore path name
    | (fp </> name) `elem` ignore = return []
    | otherwise =
        do status <- getFileStatus fp'
           analyse' status
    where
      path' = Path (SerText $ T.pack name) path
      fp' = fp </> name
      analyse' status
          | isRegularFile status =
              let CTime modtime = modificationTime status
                  COff filesize = fileSize status
              in return
                  [ FileEntry $ File
                    { f_name = path'
                    , f_size = FileSize filesize
                    , f_modtime = FileModTime modtime
                    } ]
          | isDirectory status =
              liftM ((DirectoryEntry path'):) $ analyseDirectory fp' ignore path'
          | otherwise = return [] -- No support for devices, sockets yet.

data StreamPair
    = StreamPair
    { sp_in :: InputStream ByteString
    , sp_out :: OutputStream ByteString
    }

openStreams :: Handle -> Handle -> IO StreamPair
openStreams hIn hOut =
    do inStream <- ST.handleToInputStream hIn
       outStream <- ST.handleToOutputStream hOut
       return $ StreamPair { sp_in = inStream, sp_out = outStream }

instance ProtocolM RequestMonad where
    queryHashReq = request . QueryHash
    querySetReq = request . QuerySet
    queryFileReq = request . QueryFile
    queryFileContReq = request . QueryFileCont
    logReq = request . Log
    terminateReq = request Terminate

instance ClientMonad RequestMonad where
    split = splitRequests

instance ClientMonad ServerMonad where
    split xs = liftM mconcat $ sequence xs

data Direction
    = FromRemote
    | ToRemote

child :: IO ()
child =
    do streams <- openStreams stdin stdout
       side <- getFromInputStream (sp_in streams)
       case side of
         Service dir clientServerOpts -> server dir clientServerOpts streams
         Client dir clientServerOpts -> client dir clientServerOpts streams

parent :: StreamPair -> FilePath -> FilePath -> Direction -> ClientServerOptions -> IO ()
parent streams source destination direction clientServerOpts =
    case direction of
      FromRemote ->
        do respond (sp_out streams) $ Service source clientServerOpts
           client destination clientServerOpts streams
      ToRemote ->
        do respond (sp_out streams) $ Client destination clientServerOpts
           server source clientServerOpts streams

respond :: (Show a, SE.Serialize a) => OutputStream ByteString -> a -> IO ()
respond os = mapM_ (flip ST.write os . Just) . (:[BS.empty]) . SE.encode

local :: ClientServerOptions -> FilePath -> FilePath -> IO ()
local cs source destination =
    do sourceDir <- liftM (mkTrie 0) $ analyseDirectory source (cs_ignore cs) Root
       destinationDir <- liftM (mkTrie 0) $ analyseDirectory destination (cs_ignore cs) Root
       evalStateT (abstractClient cs destination destinationDir) (startServerState source sourceDir)

server :: FilePath -> ClientServerOptions -> StreamPair -> IO ()
server fp cs streams =
    do dir <- analyseDirectory fp (cs_ignore cs) Root
       evalStateT loop (startServerState fp $ mkTrie 0 dir)
    where
       loop =
           do req <- liftIO $ getFromInputStream (sp_in streams)
              case req of
                QueryHash l -> queryHashReq l >>= (liftIO . respond (sp_out streams)) >> loop
                QuerySet l -> querySetReq l >>= (liftIO . respond (sp_out streams)) >> loop
                QueryFile f -> queryFileReq f >>= (liftIO . respond (sp_out streams)) >> loop
                QueryFileCont c -> queryFileContReq c >>= (liftIO . respond (sp_out streams)) >> loop
                Log t -> logReq t >>= (liftIO . respond (sp_out streams)) >> loop
                Terminate -> terminateReq >>= (liftIO . respond (sp_out streams)) >> return ()

client :: FilePath -> ClientServerOptions -> StreamPair -> IO ()
client fp cs streams =
    do dir <- analyseDirectory fp (cs_ignore cs) Root
       runRequestMonad (sp_in streams) (sp_out streams) $ abstractClient cs fp $ mkTrie 0 dir

