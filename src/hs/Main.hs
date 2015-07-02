{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import System.IO
import System.Posix.Files
import System.Posix.Types
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.Console.GetOpt
import Control.Monad
import Foreign.C.Types
import qualified System.IO.Streams.Handle as ST
import qualified Data.Text as T

import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import Sync.MerkleTree.Sync


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
          | otherwise = error "weird file"



syncClient :: FilePath -> [FilePath] -> IO ()
syncClient fp ignore =
    do dir <- analyseDirectory fp ignore Root
       inStream <- ST.handleToInputStream stdin
       outStream <- ST.handleToOutputStream stdout
       runSyncClient fp inStream outStream $ mkTrie 0 dir

syncServer :: FilePath -> [FilePath] -> Handle -> Handle -> IO ()
syncServer fp ignore hIn hOut =
    do trie <- liftM (mkTrie 0) $ analyseDirectory fp ignore Root
       putStrLn $ "Server finished directory traversal: " ++ (show $ t_hash trie)
       inStream <- ST.handleToInputStream hOut
       outStream <- ST.handleToOutputStream hIn
       runSyncServer fp inStream outStream trie

data SyncOptions
    = SyncOptions
      { so_base :: Maybe FilePath
      , so_ignore :: [FilePath]
      , so_client :: Bool
      }

defaultSyncOptions :: SyncOptions
defaultSyncOptions =
    SyncOptions
    { so_base = Nothing
    , so_ignore = []
    , so_client = False
    }

addIgnore :: FilePath -> SyncOptions -> SyncOptions
addIgnore fp so = so { so_ignore = fp:(so_ignore so)}

options :: [OptDescr (SyncOptions -> SyncOptions)]
options =
    [ Option ['b'] ["base"] (ReqArg (\s so -> so { so_base = Just s }) "Path") "Base"
    , Option ['i'] ["ignore"] (ReqArg addIgnore "Path") "Ignore"
    , Option ['c'] ["client"] (NoArg (\so -> so { so_client = True })) "client"
    ]

main :: IO ()
main =
    do args <- getArgs
       case (getOpt RequireOrder options args) of
         (ots,rest,[]) ->
             let syncOpts = foldl (flip ($)) defaultSyncOptions ots
             in go syncOpts rest
         (_,_,errs) ->
             do putStrLn $ usageInfo "sync" options
                putStrLn $ show errs

go :: SyncOptions -> [String] -> IO ()
go syncOpts remoteCmdLine
    | so_client syncOpts
    , null remoteCmdLine
    , Just base <- so_base syncOpts =
        syncClient base (so_ignore syncOpts)
    | (remoteCmd:remoteArgs) <- remoteCmdLine
    , not (so_client syncOpts)
    , Just base <- so_base syncOpts =
        do let cp =
                   CreateProcess
                   { cmdspec = RawCommand remoteCmd remoteArgs
                   , cwd = Nothing
                   , env = Nothing
                   , std_in = CreatePipe
                   , std_out = CreatePipe
                   , std_err = Inherit
                   , close_fds = False
                   , create_group = False
                   , delegate_ctlc = False
                   }
           (Just inH, Just outH, Nothing, _ph) <- createProcess cp
           syncServer base (so_ignore syncOpts) inH outH
    | otherwise = fail "Command line option -b is required."







