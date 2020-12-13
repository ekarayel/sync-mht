module Sync.MerkleTree.Analyse
  ( analyse,
    isRealFile,
  )
where

import Control.Exception (try)
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Foreign.C.Types
import Sync.MerkleTree.Types
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, stderr)
import System.Posix.Types
import System.PosixCompat.Files
import Text.Regex (Regex, matchRegex, mkRegex)
import Prelude hiding (lookup)

-- | Returns True if the given string is not "." or ".."
isRealFile :: String -> Bool
isRealFile x
  | x `elem` [".", ".."] = False
  | otherwise = True

shouldIgnore :: Path -> [Regex] -> Bool
shouldIgnore p regexes = any (isJust . (`matchRegex` (toFilePath "" p))) regexes

-- | Returns all files and directories below the given FilePath
analyse ::
  -- | Root file path to analyse`
  FilePath ->
  -- | List of regular expressions to be excluded from the resulting list
  [String] ->
  -- | List of file or directory entries with paths relative to the given root
  IO [Entry]
analyse fp ignore = analyseSubDirectory fp (map mkRegex ignore) Root

analyseSubDirectory :: FilePath -> [Regex] -> Path -> IO [Entry]
analyseSubDirectory fp ignore path =
  do
    files <- getDirectoryContents fp
    liftM concat $ mapM (analyseEntry fp ignore path) files

analyseEntry :: FilePath -> [Regex] -> Path -> String -> IO [Entry]
analyseEntry fp ignore path name
  | not (isRealFile name) = return []
  | shouldIgnore path' ignore = return []
  | otherwise =
    lstat fp' >>= maybe (return []) analyse'
  where
    path' = Path (T.pack name) path
    fp' = fp </> name
    analyse' status
      | isRegularFile status =
        let CTime modtime = modificationTime status
            COff filesize = fileSize status
         in return
              [ FileEntry $
                  File
                    { f_name = path',
                      f_size = FileSize filesize,
                      f_modtime = FileModTime $ fromIntegral modtime
                    }
              ]
      | isDirectory status =
        liftM ((DirectoryEntry path') :) $ analyseSubDirectory fp' ignore path'
      | otherwise = return [] -- No support for devices, sockets yet.

lstat :: FilePath -> IO (Maybe FileStatus)
lstat p = do
  ls <- try (getSymbolicLinkStatus p) :: IO (Either IOError FileStatus)
  case ls of
    Left e -> do
      hPutStrLn stderr $ show e
      return Nothing
    Right s -> return $ Just s
