{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Analyse
    ( analyse
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

-- | Returns True if the given string is not "." or ".."
isRealFile :: String -> Bool
isRealFile x
    | x `elem` [".", ".."] = False
    | otherwise = True

shouldIgnore :: Path -> [Regex] -> Bool
shouldIgnore p regexes = any (isJust . (`matchRegex` (toFilePath "" p))) regexes

-- | Returns all files and directories below the given FilePath
analyse ::
    FilePath -- ^ Root file path to analyse`
    -> [String] -- ^ List of regular expressions to be excluded from the resulting list
    -> IO [Entry] -- ^ List of file or directory entries with paths relative to the given root
analyse fp ignore = analyseSubDirectory fp (map mkRegex ignore) Root

analyseSubDirectory :: FilePath -> [Regex] -> Path -> IO [Entry]
analyseSubDirectory fp ignore path =
    do files <- getDirectoryContents fp
       liftM concat $ mapM (analyseEntry fp ignore path) $ filter isRealFile files

analyseEntry :: FilePath -> [Regex] -> Path -> String -> IO [Entry]
analyseEntry fp ignore path name
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
              liftM ((DirectoryEntry path'):) $ analyseSubDirectory fp' ignore path'
          | otherwise = return [] -- No support for devices, sockets yet.
