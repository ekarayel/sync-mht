{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Sync
    ( client
    , server
    ) where

import Control.Monad

import System.FilePath
import Foreign.C.Types
import Prelude hiding (lookup)
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import System.Directory
import System.IO
import System.Posix.Types
import System.Posix.Files
import qualified Data.Text as T
import qualified System.IO.Streams as ST

import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Client
import Sync.MerkleTree.Server

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

client :: FilePath -> [FilePath] -> IO ()
client = run runClient stdin stdout

server :: Handle -> Handle -> FilePath -> [FilePath] -> IO ()
server = run runServer

run :: RunSide -> Handle -> Handle -> FilePath -> [FilePath] -> IO ()
run runSide hIn hOut fp ignore =
    do dir <- analyseDirectory fp ignore Root
       inStream <- ST.handleToInputStream hOut
       outStream <- ST.handleToOutputStream hIn
       runSide fp inStream outStream $ mkTrie 0 dir





