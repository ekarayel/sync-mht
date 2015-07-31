{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Analyse
    ( analyseDirectory
    , isRealFile
    ) where

import Control.Monad
import Data.Maybe
import System.FilePath
import Foreign.C.Types
import Prelude hiding (lookup)
import Sync.MerkleTree.Types
import System.Directory
import System.Posix.Types
import System.Posix.Files
import Text.Regex (matchRegex, mkRegex, Regex)
import qualified Data.Text as T

isRealFile :: String -> Bool
isRealFile x
    | x `elem` [".", ".."] = False
    | otherwise = True

shouldIgnore :: Path -> [Regex] -> Bool
shouldIgnore p regexes = any (isJust . (`matchRegex` (toFilePath "" p))) regexes

analyseDirectory :: FilePath -> [String] -> Path -> IO [Entry]
analyseDirectory fp ignore path = analyseDir fp (map mkRegex ignore) path

analyseDir :: FilePath -> [Regex] -> Path -> IO [Entry]
analyseDir fp ignore path
    | shouldIgnore path ignore = return []
    | otherwise =
        do files <- getDirectoryContents fp
           liftM concat $ mapM (analyse fp ignore path) $ filter isRealFile files

analyse :: FilePath -> [Regex] -> Path -> String -> IO [Entry]
analyse fp ignore path name
    | shouldIgnore path' ignore = return []
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
              liftM ((DirectoryEntry path'):) $ analyseDir fp' ignore path'
          | otherwise = return [] -- No support for devices, sockets yet.

