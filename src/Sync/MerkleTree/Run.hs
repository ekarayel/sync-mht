module Sync.MerkleTree.Run where

import Control.Concurrent
import Control.Concurrent.MVar ()
import Control.Monad
import Data.List
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import qualified Paths_sync_mht as P
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Sync
import System.Console.GetOpt
import System.Exit
import System.IO
import System.IO.Error
import System.Process

data SyncOptions = SyncOptions
  { so_source :: Maybe FilePath,
    so_destination :: Maybe FilePath,
    so_remote :: Maybe String,
    so_ignore :: [String],
    so_boring :: [FilePath],
    so_add :: Bool,
    so_update :: Bool,
    so_delete :: Bool,
    so_help :: Bool,
    so_nonOptions :: [String],
    so_compareClocks :: Maybe (Double, Double),
    so_version :: Bool
  }

defaultSyncOptions :: SyncOptions
defaultSyncOptions =
  SyncOptions
    { so_source = Nothing,
      so_destination = Nothing,
      so_remote = Nothing,
      so_ignore = [],
      so_boring = [],
      so_add = False,
      so_update = False,
      so_delete = False,
      so_help = False,
      so_nonOptions = [],
      so_version = False,
      so_compareClocks = Nothing
    }

withClientServerOptions :: SyncOptions -> ((?clientServerOptions :: ClientServerOptions) => IO a) -> IO a
withClientServerOptions so action =
  do
    let parseBoringFile = map T.unpack . filter noComment . map T.strip . T.lines
        noComment s = not (T.null s || ("#" `T.isPrefixOf` s))
    ignoreFromBoringFiles <-
      forM (so_boring so) $ liftM parseBoringFile . T.readFile
    let ?clientServerOptions =
          ClientServerOptions
            { cs_add = so_add so,
              cs_update = so_update so,
              cs_delete = so_delete so,
              cs_ignore = (concat ignoreFromBoringFiles) ++ (so_ignore so),
              cs_compareClocks = so_compareClocks so
            }
    action

optDescriptions :: [OptDescr (SyncOptions -> SyncOptions)]
optDescriptions =
  [ Option
      ['s']
      ["source"]
      (ReqArg (\fp so -> so {so_source = Just fp}) "DIR")
      "source directory",
    Option
      ['d']
      ["destination"]
      (ReqArg (\fp so -> so {so_destination = Just fp}) "DIR")
      "destination directory",
    Option
      ['r']
      ["remote-shell"]
      (ReqArg (\s so -> so {so_remote = Just s}) "CMD")
      "synchroize with a remote-site (see below)",
    Option
      ['i']
      ["ignore"]
      (ReqArg (\fp so -> so {so_ignore = fp : (so_ignore so)}) "REGEX")
      "ignore entries matching the given regex",
    Option
      ['b']
      ["boring"]
      (ReqArg (\fp so -> so {so_boring = fp : (so_boring so)}) "PATH")
      "ignore entries matching the regexes in the given file",
    Option
      ['a']
      ["add"]
      (NoArg (\so -> so {so_add = True}))
      "copy additional files from the source directory",
    Option
      ['u']
      ["update"]
      (NoArg (\so -> so {so_update = True}))
      "overwrite existing files",
    Option
      []
      ["delete"]
      (NoArg (\so -> so {so_delete = True}))
      "delete superfluos files in the destination directory",
    Option
      []
      ["compareclocks"]
      (OptArg (\x so -> so {so_compareClocks = fmap (flip (,) 0.0 . read) x}) "T")
      "check whether there is a clock drift between client and server",
    Option ['v'] ["version"] (NoArg (\so -> so {so_version = True})) "shows version",
    Option ['h'] ["help"] (NoArg (\so -> so {so_help = True})) "shows usage information"
  ]

parseNonOption :: String -> (SyncOptions -> SyncOptions)
parseNonOption s so = so {so_nonOptions = s : (so_nonOptions so)}

toSyncOptions :: [(SyncOptions -> SyncOptions)] -> SyncOptions
toSyncOptions = foldl (flip id) defaultSyncOptions

putError :: String -> IO ()
putError = hPutStrLn stderr

printUsageInfo :: IO ()
printUsageInfo =
  mapM_ putError ([usageInfo header optDescriptions] ++ [details])
  where
    header =
      unlines
        [ "Usage: sync-mht [OPTIONS..]",
          "",
          "Fast incremental file transfer using Hash-Trees (Version: " ++ version ++ ")"
        ]
    details =
      unlines
        [ "Note: The argument to the --remote-shell option should be a CMD running sync-mht",
          "with a remote command execution tool (like ssh or docker). If given exactly one of",
          "the directories must be prepended with 'remote:' - indicating a folder on the site,",
          "accessible with the provided remote shell command."
        ]

data Location
  = Remote FilePath
  | Local FilePath

parseFilePath :: FilePath -> Location
parseFilePath fp
  | Just rest <- stripPrefix "remote:" fp = Remote rest
  | otherwise = Local fp

main :: [String] -> IO ()
main args = flip catchIOError (die . show) $
  do
    let parsedOpts = getOpt (ReturnInOrder parseNonOption) optDescriptions args
    if
        | null args -> runChild
        | (options, [], []) <- parsedOpts ->
          do
            mMsg <- run $ toSyncOptions options
            case mMsg of
              Just err -> die $ T.unpack err
              Nothing -> return ()
        | (_, _, errs) <- parsedOpts -> die $ unlines errs

version :: String
version = showVersion P.version

run :: SyncOptions -> IO (Maybe T.Text)
run so
  | so_help so = printUsageInfo >> return Nothing
  | so_version so = putStrLn version >> return Nothing
  | not (null (so_nonOptions so)) =
    return $
      Just $
        T.concat
          [ "Unrecognized options: ",
            T.intercalate ", " (map T.pack $ so_nonOptions so)
          ]
  | Just source <- so_source so,
    Just destination <- so_destination so =
    withClientServerOptions so $ do
      case (parseFilePath source, parseFilePath destination) of
        (Remote _, Remote _) -> return $ Just doubleRemote
        (Local source', Local destination')
          | Just _ <- so_remote so -> return $ Just missingRemote
          | otherwise -> local source' destination'
        (Remote source', Local destination')
          | Just remoteCmd <- so_remote so ->
            runParent remoteCmd source' destination' FromRemote
          | otherwise -> return $ Just missingRemoteCmd
        (Local source', Remote destination')
          | Just remoteCmd <- so_remote so ->
            runParent remoteCmd source' destination' ToRemote
          | otherwise -> return $ Just missingRemoteCmd
  | otherwise =
    do
      let missingOpts =
            T.intercalate ", " $
              map snd $
                filter
                  ((== Nothing) . ($ so) . fst)
                  [(so_source, "--source"), (so_destination, "--destination")]
      return $ Just $ [i|The options #{missingOpts} are required.|]
  where
    doubleRemote = "Either the directory given in --source or --destination must be local."
    missingRemote =
      T.concat
        [ "The --remote-shell options requires that either the directory given at ",
          "--source or --destination is at remote site. (Indicated by the prefix: 'remote:')"
        ]
    missingRemoteCmd = "The --remote-shell is required when the prefix 'remote:' is used."

-- | Number of micro-seconds to wait for communciation from a parent process,
-- before suspecting that the user may have launched the command without parameters.
waitForInput :: Int
waitForInput = 1000 * 1000 * 3

-- | Run as a child process communicating with a parent process.
runChild :: IO ()
runChild =
  do
    gotMessage <- newEmptyMVar
    streams <- openStreams stdin stdout
    _ <- forkIO $
      do
        threadDelay waitForInput
        r <- isEmptyMVar gotMessage
        when r $
          putError
            "Running in server mode. (The command `sync-mht --help` prints usage info.)"
    child gotMessage streams

-- | Run as a parent process communicating with a child process.
runParent ::
  (?clientServerOptions :: ClientServerOptions) =>
  String ->
  FilePath ->
  FilePath ->
  Direction ->
  IO (Maybe T.Text)
runParent remoteCmd source destination dir =
  do
    (Just hIn, Just hOut, Nothing, ph) <-
      createProcess $
        (shell remoteCmd)
          { std_in = CreatePipe,
            std_out = CreatePipe
          }
    parentStreams <- openStreams hOut hIn
    exitMsg <- parent parentStreams source destination dir
    hClose hIn
    hClose hOut
    _ <- waitForProcess ph
    return exitMsg
