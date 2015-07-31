{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Run where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Version (showVersion)
import Paths_sync_mht (version)
import System.Console.GetOpt
import System.IO
import System.Process
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Sync
import qualified Data.Text as T
import qualified Data.Text.IO as T

data RemoteCmd
    = RemoteCmd String
    | Simulate

data SyncOptions
    = SyncOptions
      { so_source :: Maybe FilePath
      , so_destination :: Maybe FilePath
      , so_remote :: Maybe RemoteCmd
      , so_ignore :: [String]
      , so_boring :: [FilePath]
      , so_add :: Bool
      , so_update :: Bool
      , so_delete :: Bool
      , so_help :: Bool
      , so_nonOptions :: [String]
      }

defaultSyncOptions :: SyncOptions
defaultSyncOptions =
    SyncOptions
    { so_source = Nothing
    , so_destination = Nothing
    , so_remote = Nothing
    , so_ignore = []
    , so_boring = []
    , so_add = False
    , so_update = False
    , so_delete = False
    , so_help = False
    , so_nonOptions = []
    }

toClientServerOptions :: SyncOptions -> IO ClientServerOptions
toClientServerOptions so =
     do let parseBoringFile = map T.unpack . filter noComment . map T.strip . T.lines
            noComment s = not (T.null s || ("#" `T.isPrefixOf` s))
        ignoreFromBoringFiles <-
            forM (so_boring so) $ liftM parseBoringFile . T.readFile
        return $
            ClientServerOptions
            { cs_add = so_add so
            , cs_update = so_update so
            , cs_delete = so_delete so
            , cs_ignore = (concat ignoreFromBoringFiles) ++ (so_ignore so)
            }

optDescriptions :: [OptDescr (SyncOptions -> SyncOptions)]
optDescriptions =
    [ Option ['s'] ["source"] (ReqArg (\fp so -> so { so_source = Just fp }) "DIR")
        "directory to copy files from (source) (this option is required)"
    , Option ['d'] ["destination"] (ReqArg (\fp so -> so { so_destination = Just fp }) "DIR")
        "directory to copy files to (destination) (this option is required)"
    , Option ['r'] ["remote-shell"] (ReqArg (\s so -> so { so_remote = Just $ RemoteCmd s }) "CMD")
        "synchroize with a remote-site using a remote command execution tool (like ssh or docker)"
    , Option ['i'] ["ignore"] (ReqArg (\fp so -> so { so_ignore = fp:(so_ignore so) }) "REGEX")
        $ concat
        [ "regular expression for files or directories (relative to the source and destination "
        , "directories) that are to be ignored during synchroization (this option can be given "
        , " multiple times)"
        ]
    , Option ['b'] ["boring"] (ReqArg (\fp so -> so { so_boring = fp:(so_boring so) }) "PATH")
        $ concat
        [ "boring file - darcs format"
        ]    , Option ['a'] ["add"] (NoArg (\so -> so { so_add = True })) ( concat
        [ "copy files from the source directory if there is corresponding file inside "
        , "the destination directory"
        ])
    , Option ['u'] ["update"] (NoArg (\so -> so { so_update = True })) ( concat
        [ "overwrite existing files inside the destination directory if their content does not "
        , "match the respective files inside the source directory"
        ])
    , Option [] ["delete"] (NoArg (\so -> so { so_delete = True })) ( concat
        [ "delete files inside the destination directory if there is no corresponding file inside "
        , "the source directory"
        ])
    , Option ['h'] ["help"] (NoArg (\so -> so { so_help = True })) "shows usage information"
    ]

parseNonOption :: String -> (SyncOptions -> SyncOptions)
parseNonOption s so = so { so_nonOptions = s:(so_nonOptions so) }

toSyncOptions :: [(SyncOptions -> SyncOptions)] -> SyncOptions
toSyncOptions = foldl (flip id) defaultSyncOptions

putError :: String -> IO ()
putError = hPutStrLn stderr

_HIDDENT_CLIENT_MODE_OPTION_ :: String
_HIDDENT_CLIENT_MODE_OPTION_ = "--hidden-client-mode-option"

printUsageInfo :: [String] -> IO ()
printUsageInfo prefix = mapM_ putError (prefix ++ [usageInfo header optDescriptions] ++ [details])
    where
      header = unlines
          [ "sync-mht version " ++ showVersion version
          , ""
          , "Usage: sync-mht [OPTIONS..]"
          ]
      details = concat
          [ "Note: If the --remote-shell option has been provided, exactly one of the directories "
          , "must be prepended with 'remote:' - indicating a folder on the site, accessible with "
          , "the provided remote shell command."
          ]

data Location
    = Remote FilePath
    | Local FilePath

parseFilePath :: FilePath -> Location
parseFilePath fp
    | Just rest <- stripPrefix "remote:" fp = Remote rest
    | otherwise = Local fp

run :: SyncOptions -> IO ()
run so
    | so_help so =
        printUsageInfo []
    | not (null (so_nonOptions so)) =
        printUsageInfo ["Could not understand the following options: " ++ show (so_nonOptions so)]
    | Just source <- so_source so, Just destination <- so_destination so =
        do cs <- toClientServerOptions so
           case (parseFilePath source, parseFilePath destination) of
             (Remote _, Remote _) -> printUsageInfo [doubleRemote]
             (Local source', Local destination')
                 | Just _ <- so_remote so -> printUsageInfo [missingRemote]
                 | otherwise -> local cs source' destination'
             (Remote source', Local destination')
                 | Just remoteCmd <- so_remote so ->
                     runParent cs remoteCmd source' destination' FromRemote
                 | otherwise -> printUsageInfo [missingRemoteCmd]
             (Local source', Remote destination')
                 | Just remoteCmd <- so_remote so ->
                     runParent cs remoteCmd source' destination' ToRemote
                 | otherwise -> printUsageInfo [missingRemoteCmd]
    | otherwise =
        do let missingOpts =
                intercalate ", " $ map snd $ filter ((== Nothing) . ($ so) . fst)
                [(so_source, "--source"),(so_destination, "--destination")]
           printUsageInfo ["The follwing required options: " ++ missingOpts ++ " were missing."]
    where
      doubleRemote = "Either the directory given in --source or --destination must be local."
      missingRemote = concat
          [ "The --remote-shell options requires that either the directory given at "
          , "--source or --destination is at remote site. (Indicated by the prefix: 'remote:')"
          ]
      missingRemoteCmd = "The --remote-shell is required when the prefix 'remote:' is used."

runChild :: IO ()
runChild =
     do streams <- openStreams stdin stdout
        child streams

runParent :: ClientServerOptions -> RemoteCmd -> FilePath -> FilePath -> Direction -> IO ()
runParent clientServerOpts mRemoteCmd source destination dir =
    case mRemoteCmd of
      RemoteCmd remoteCmd ->
          do let remoteCmd' = remoteCmd ++ " " ++ _HIDDENT_CLIENT_MODE_OPTION_
             handles <-
                 createProcess ((shell remoteCmd') { std_in = CreatePipe, std_out = CreatePipe })
             case handles of
               (Just hIn, Just hOut, Nothing, _ph) ->
                   do streams <- openStreams hOut hIn
                      parent streams source destination dir clientServerOpts
               _ -> fail "createProcess did not return the correct set of handles."
      Simulate ->
          do (parentInStream, childOutStream) <- mkChanStreams
             (childInStream, parentOutStream) <- mkChanStreams
             _ <- forkIO $ child $ StreamPair { sp_in = childInStream, sp_out = childOutStream }
             let parentStreams = StreamPair { sp_in = parentInStream, sp_out = parentOutStream }
             parent parentStreams source destination dir clientServerOpts
