{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Run where

import Control.Concurrent
import Control.Monad
import Data.List

import System.Console.GetOpt
import System.IO
import System.IO.Error
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
        "source directory"
    , Option ['d'] ["destination"] (ReqArg (\fp so -> so { so_destination = Just fp }) "DIR")
        "destination directory"
    , Option ['r'] ["remote-shell"] (ReqArg (\s so -> so { so_remote = Just $ RemoteCmd s }) "CMD")
        "synchroize with a remote-site (see below)"
    , Option ['i'] ["ignore"] (ReqArg (\fp so -> so { so_ignore = fp:(so_ignore so) }) "REGEX")
        "ignore entries matching the given regex"
    , Option ['b'] ["boring"] (ReqArg (\fp so -> so { so_boring = fp:(so_boring so) }) "PATH")
        "ignore entries matching the regexes in the given file"
    , Option ['a'] ["add"] (NoArg (\so -> so { so_add = True }))
        "copy additional files from the source directory"
    , Option ['u'] ["update"] (NoArg (\so -> so { so_update = True }))
        "overwrite existing files"
    , Option [] ["delete"] (NoArg (\so -> so { so_delete = True }))
        "delete superfluos files in the destination directory"
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

printUsageInfo :: String -> [String] -> IO ()
printUsageInfo version prefix =
    mapM_ putError (prefix ++ [usageInfo header optDescriptions] ++ [details])
    where
      header = unlines
          [ "Usage: sync-mht [OPTIONS..]"
          , ""
          , "Fast incremental file transfer using Merkle-Hash-Trees (Version: " ++ version ++ ")"
          ]
      details = unlines
          [ "Note: The argument to the --remote-shell option should be a CMD running sync-mht"
          , "with a remote command execution tool (like ssh or docker). If given exactly one of"
          , "the directories must be prepended with 'remote:' - indicating a folder on the site,"
          , "accessible with the provided remote shell command."
          ]

data Location
    = Remote FilePath
    | Local FilePath

parseFilePath :: FilePath -> Location
parseFilePath fp
    | Just rest <- stripPrefix "remote:" fp = Remote rest
    | otherwise = Local fp

main :: String -> [String] -> IO ()
main version args = flip catchIOError (putError . show) $
    do let parsedOpts = getOpt (ReturnInOrder parseNonOption) optDescriptions args
       case () of
         () | [_HIDDENT_CLIENT_MODE_OPTION_] == args -> runChild
            | (options,[],[]) <- parsedOpts -> run version $ toSyncOptions options
            | (_,_,errs) <- parsedOpts -> printUsageInfo version errs

run :: String -> SyncOptions -> IO ()
run version so
    | so_help so = usage []
    | not (null (so_nonOptions so)) =
        usage ["Unrecognized options: " ++ intercalate ", " (so_nonOptions so)]
    | Just source <- so_source so, Just destination <- so_destination so =
        do cs <- toClientServerOptions so
           case (parseFilePath source, parseFilePath destination) of
             (Remote _, Remote _) -> usage [doubleRemote]
             (Local source', Local destination')
                 | Just _ <- so_remote so -> usage [missingRemote]
                 | otherwise -> local cs source' destination'
             (Remote source', Local destination')
                 | Just remoteCmd <- so_remote so ->
                     runParent cs remoteCmd source' destination' FromRemote
                 | otherwise -> usage [missingRemoteCmd]
             (Local source', Remote destination')
                 | Just remoteCmd <- so_remote so ->
                     runParent cs remoteCmd source' destination' ToRemote
                 | otherwise -> usage [missingRemoteCmd]
    | otherwise =
        do let missingOpts =
                intercalate ", " $ map snd $ filter ((== Nothing) . ($ so) . fst)
                [(so_source, "--source"),(so_destination, "--destination")]
           usage ["The options " ++ missingOpts ++ " are required."]
    where
      usage = printUsageInfo version
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
    do parentStreams <-
           case mRemoteCmd of
             RemoteCmd remoteCmd ->
                 do let remoteCmd' = remoteCmd ++ " " ++ _HIDDENT_CLIENT_MODE_OPTION_
                    (Just hIn, Just hOut, Nothing, _ph) <-
                        createProcess $ (shell remoteCmd')
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        }
                    openStreams hOut hIn
             Simulate ->
                 do (parentInStream, childOutStream) <- mkChanStreams
                    (childInStream, parentOutStream) <- mkChanStreams
                    _ <- forkIO $ child $
                        StreamPair
                        { sp_in = childInStream
                        , sp_out = childOutStream 
                        }
                    return $ StreamPair { sp_in = parentInStream, sp_out = parentOutStream }
       parent parentStreams source destination dir clientServerOpts
