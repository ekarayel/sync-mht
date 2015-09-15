{-# LANGUAGE DeriveGeneric #-}
import Control.Monad
import Data.Aeson hiding (Result)
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Ratio
import Data.Time.Clock
import GHC.Generics
import System.Directory
import System.IO.Temp
import System.FilePath
import System.Process
import System.Environment

data Times
    = Times
      { t_syncMht :: Integer
      , t_rsync :: Integer
      , t_size :: Integer
      }
      deriving Generic
instance ToJSON Times

data Result
    = Result
      { r_full :: Times
      , r_patch :: Times
      , r_sample :: Sample
      }
      deriving Generic
instance ToJSON Result

data Summary
    = Summary
    { s_results :: [Result]
    , s_tag :: String
    }
    deriving Generic
instance ToJSON Summary

_RESOLUTION_ :: (Fractional a) => a
_RESOLUTION_ = fromRational $ 1000 % 1

withTime :: IO () -> IO Integer
withTime action =
    do start <- getCurrentTime
       action
       end <- getCurrentTime
       return $ round $ _RESOLUTION_ * (diffUTCTime end start)

runCmd :: [String] -> IO ()
runCmd = callCommand . intercalate " "

_SSH_PREFIX_ :: String
_SSH_PREFIX_ = "ssh"

_LOCALHOST_ :: String
_LOCALHOST_ = "127.0.0.1"

realFile :: String -> Bool
realFile = (`notElem` [".", ".."])

data Sample
    = Sample
    { s_ghAccount :: String
    , s_ghRepo :: String
    , s_startTag :: String
    , s_diffTag :: String
    }
    deriving Generic
instance ToJSON Sample

samples :: [Sample]
samples =
    [ Sample
      { s_ghAccount = "gionkunz"
      , s_ghRepo = "chartist-js"
      , s_startTag = "60e94d35e2e023f90e14b8907eb1115c9270e7ad"
      , s_diffTag = "ffddebc1c819f14d3982af5c185f9f619249020c"
      }
    , Sample
      { s_ghAccount = "JuliaLang"
      , s_ghRepo = "julia"
      , s_startTag = "3cb9b4879d9da4e14caa79fa6a1ed71fa93871e7"
      , s_diffTag = "5b40f2bd35d87b267dc1ff1cfc6fd7e07670e89c"
      }
    , Sample
      { s_ghAccount = "google"
      , s_ghRepo = "closure-compiler"
      , s_startTag = "2b748b89f8737f3338c0cbe027aa09df1dc8fcaa"
      , s_diffTag = "7e74ada6effa6d429401e2f363a79594a123f269"
      }
    , Sample
      { s_ghAccount = "atom"
      , s_ghRepo = "atom"
      , s_diffTag = "321bf690d214d229b132a4bc03c2f249aa670340"
      , s_startTag = "27cc9cdff3521d5fd5b1725f8e354a2df91117b1"
      }
    ]

sourceDir :: FilePath -> FilePath
sourceDir = (</> "src")

sourceDataDir :: FilePath -> Sample -> FilePath
sourceDataDir baseDir s = (sourceDir baseDir) </> (s_ghRepo s ++ "-" ++ s_startTag s)

targetDir :: FilePath -> String -> FilePath
targetDir fp s = (fp </> "src" </> s)

prepare :: Sample -> FilePath -> IO (FilePath, Integer, Integer)
prepare s benchmarks =
    do createDirectoryIfMissing True benchmarks
       createDirectory baseDir
       createDirectory src
       mapM (createDirectory . targetDir baseDir) ["rsync", "sync-mht"]
       mapM (runCmd . (["wget","-P",src]++) . (:[]) . (gitHubPrefix ++))
           [ "/archive/" ++ startTagZip
           , "/commit/" ++ (s_diffTag s) ++ ".diff"
           ]
       runCmd ["unzip", src </> startTagZip, "-d", src]
       fSize <- readCreateProcess
           (shell "zip -0 -r - . | wc -c") { cwd = Just $ sourceDataDir baseDir s } ""
       pSize <- readCreateProcess
           (shell $ concat ["cat ", sourceDir baseDir </> (s_diffTag s ++ ".diff"), " | wc -c"]) ""
       return (baseDir, read fSize, read pSize)
    where
      gitHubPrefix = "https://github.com/" ++ (s_ghAccount s) ++ "/" ++ (s_ghRepo s)
      startTagZip = (s_startTag s) ++ ".zip"
      src = sourceDir baseDir
      baseDir = benchmarks </> (s_ghAccount s ++ "-" ++ s_ghRepo s)

measure :: Integer -> String -> Sample -> FilePath -> IO Times
measure size syncMhtPath s baseDir =
    do let srcData = sourceDataDir baseDir s
       tSyncMht <- withTime $ runCmd
           [ syncMhtPath, "-a", "-u", "--delete", "-s", srcData, "-d"
           , "remote:" ++ (targetDir baseDir "sync-mht"), "-r"
           , "'" ++ _SSH_PREFIX_ ++ " " ++ _LOCALHOST_ ++ " " ++ syncMhtPath ++ "'"
           ]
       tRsync <- withTime $ runCmd
           [ "rsync", "-rtz", "--delete", "-e", "'"++_SSH_PREFIX_++"'", srcData
           , _LOCALHOST_ ++ ":" ++ (targetDir baseDir "rsync")
           ]
       return $
           Times
           { t_syncMht = tSyncMht
           , t_rsync = tRsync
           , t_size = size
           }

-- | Compare synchronization speeds of rsync vs. sync-mht using (patches of) GitHub repositories
-- Note: To make the comparison fair, we need to run sync-mht (just as rsync using a shell call).
-- Requires: patch, ssh, sshd listening on port 22, passwordless login for loopback ssh
main :: IO ()
main = withSystemTempDirectory "bench" $ \benchmarks ->
    do [syncMhtPath,tag] <- getArgs
       results <- forM samples $ \s ->
           do (baseDir, fSize, pSize) <- prepare s benchmarks
              fullTimes <- measure fSize syncMhtPath s baseDir
              runCmd
                  [ "patch", "-p1", "-d", sourceDataDir baseDir s, "-i"
                  , sourceDir baseDir </> (s_diffTag s ++ ".diff")
                  ]
              patchTimes <- measure pSize syncMhtPath s baseDir
              return $
                  Result
                  { r_full = fullTimes
                  , r_patch = patchTimes
                  , r_sample = s
                  }
       BS.writeFile "benchmarks.json" $ encode $
           Summary
           { s_results = results
           , s_tag = tag
           }
