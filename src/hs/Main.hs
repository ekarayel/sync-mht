{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.Process
import System.IO
import System.IO.Error
import System.Console.GetOpt
import Paths_sync_mht (version)
import Data.Version (showVersion)

import Sync.MerkleTree.Sync

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

toSyncOptions :: [(SyncOptions -> SyncOptions)] -> SyncOptions
toSyncOptions = foldl (flip id) defaultSyncOptions

putError :: String -> IO ()
putError = hPutStrLn stderr

main :: IO ()
main =
    do args <- getArgs
       case (getOpt RequireOrder options args) of
         (options,rest,[]) -> catchIOError (run (toSyncOptions options) rest) (putError . show)
         (_,_,errs) -> mapM_ putError (errs ++ [usageInfo header options])
    where
      header = concat
          [ "sync-mht version ", showVersion version, "\n"
          , "Usage: sync-mht [OPTIONS..] command"
          ]

run :: SyncOptions -> [String] -> IO ()
run syncOpts remoteCmdLine
    | so_client syncOpts, null remoteCmdLine, Just base <- so_base syncOpts =
        client base (so_ignore syncOpts)
    | (cmd:args) <- remoteCmdLine, not (so_client syncOpts), Just base <- so_base syncOpts =
        do let cp = (proc cmd args) { std_in = CreatePipe, std_out = CreatePipe }
           handles <- createProcess cp
           case handles of
             (Just inH, Just outH, Nothing, _ph) -> server inH outH base (so_ignore syncOpts)
             _ -> fail "TODO"
    | otherwise = fail "Command line option -b is required."

