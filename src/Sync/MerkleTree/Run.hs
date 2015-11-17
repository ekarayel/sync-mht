{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Run where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Exit
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
      , so_compareClocks :: Maybe (Double, Double)
      , so_version :: Bool
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
    , so_version = False
    , so_compareClocks = Nothing
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
            , cs_compareClocks = so_compareClocks so
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
    , Option [] ["compareclocks"]
        (OptArg (\x so -> so { so_compareClocks = fmap (flip (,) 0.0 . read) x }) "T")
        "check whether there is a clock drift between client and server"
    , Option ['v'] ["version"] (NoArg (\so -> so { so_version = True})) "shows version"
    , Option ['h'] ["help"] (NoArg (\so -> so { so_help = True })) "shows usage information"
    ]

parseNonOption :: String -> (SyncOptions -> SyncOptions)
parseNonOption s so = so { so_nonOptions = s:(so_nonOptions so) }

toSyncOptions :: [(SyncOptions -> SyncOptions)] -> SyncOptions
toSyncOptions = foldl (flip id) defaultSyncOptions

putError :: String -> IO ()
putError = hPutStrLn stderr

printUsageInfo :: String -> IO ()
printUsageInfo version =
    mapM_ putError ([usageInfo header optDescriptions] ++ [details])
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
           exit err = hPutStrLn stderr err >> exitFailure
       case () of
         () | [] == args -> runChild
            | (options,[],[]) <- parsedOpts ->
                do mMsg <- run version $ toSyncOptions options
                   case mMsg of
                     Just err -> exit $ T.unpack err
                     Nothing -> return ()
            | (_,_,errs) <- parsedOpts -> exit $ concat $ map (++"\n") errs

run :: String -> SyncOptions -> IO (Maybe T.Text)
run version so
    | so_help so = usage >> return Nothing
    | so_version so = putStrLn version >> return Nothing
    | not (null (so_nonOptions so)) =
        return $ Just $ T.concat
            [ "Unrecognized options: "
            , T.intercalate ", " (map T.pack $ so_nonOptions so)
            ]
    | Just source <- so_source so, Just destination <- so_destination so =
        do cs <- toClientServerOptions so
           case (parseFilePath source, parseFilePath destination) of
             (Remote _, Remote _) -> return $ Just doubleRemote
             (Local source', Local destination')
                 | Just _ <- so_remote so -> return $ Just missingRemote
                 | otherwise -> local cs source' destination'
             (Remote source', Local destination')
                 | Just remoteCmd <- so_remote so ->
                     runParent cs remoteCmd source' destination' FromRemote
                 | otherwise -> return $ Just missingRemoteCmd
             (Local source', Remote destination')
                 | Just remoteCmd <- so_remote so ->
                     runParent cs remoteCmd source' destination' ToRemote
                 | otherwise -> return $ Just missingRemoteCmd
    | otherwise =
        do let missingOpts =
                T.intercalate ", " $ map snd $ filter ((== Nothing) . ($ so) . fst)
                [(so_source, "--source"), (so_destination, "--destination")]
           return $ Just $ T.concat [ "The options ", missingOpts, " are required." ]
    where
      usage = printUsageInfo version
      doubleRemote = "Either the directory given in --source or --destination must be local."
      missingRemote = T.concat
          [ "The --remote-shell options requires that either the directory given at "
          , "--source or --destination is at remote site. (Indicated by the prefix: 'remote:')"
          ]
      missingRemoteCmd = "The --remote-shell is required when the prefix 'remote:' is used."

_WAIT_FOR_INPUT_ :: Int
_WAIT_FOR_INPUT_ = 1000 * 1000 * 3

runChild :: IO ()
runChild =
     do gotMessage <- newEmptyMVar
        streams <- openStreams stdin stdout
        _ <- forkIO $
            do threadDelay _WAIT_FOR_INPUT_
               r <- isEmptyMVar gotMessage
               when r $ putError
                   "Running in server mode. (The command `sync-mht --help` prints usage info.)"
        child gotMessage streams

runParent ::
    ClientServerOptions
    -> RemoteCmd
    -> FilePath
    -> FilePath
    -> Direction
    -> IO (Maybe T.Text)
runParent clientServerOpts mRemoteCmd source destination dir =
    do (exitAction, parentStreams) <-
           case mRemoteCmd of
             RemoteCmd remoteCmd ->
                 do (Just hIn, Just hOut, Nothing, ph) <-
                        createProcess $ (shell remoteCmd)
                        { std_in = CreatePipe
                        , std_out = CreatePipe
                        }
                    parentStreams <- openStreams hOut hIn
                    let shutdown =
                            do hClose hIn
                               hClose hOut
                               waitForProcess ph
                               return ()
                    return (shutdown, parentStreams)
             Simulate ->
                 do (parentInStream, childOutStream) <- mkChanStreams
                    (childInStream, parentOutStream) <- mkChanStreams
                    childTerminated <- newEmptyMVar
                    running <- newEmptyMVar
                    let childStrs = StreamPair { sp_in = childInStream, sp_out = childOutStream }
                    let parentStrs = StreamPair { sp_in = parentInStream, sp_out = parentOutStream }
                    _ <- forkFinally (child running childStrs) (const $ putMVar childTerminated ())
                    return (takeMVar childTerminated, parentStrs)
       exitMsg <- parent parentStreams source destination dir clientServerOpts
       exitAction
       return exitMsg
