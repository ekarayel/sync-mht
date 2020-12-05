{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Sync.MerkleTree.RunSpec (spec) where

import Test.Hspec

import Control.Exception
import Control.Monad
import Data.List
import Data.Ix
import Foreign.C.Types
import Sync.MerkleTree.Analyse
import Sync.MerkleTree.Client
import Sync.MerkleTree.Run hiding (Local)
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import System.IO.Temp
import System.PosixCompat.Files
import qualified Data.Text as T

spec :: Spec
spec = 
   describe "sync-mht" $ do
      it "verifies clock drift" $ withTempDir verifyClockDrift
      it "complains when unknown options are provided" $ runVariations unknownOptionTest
      it "validates options" $ withTempDir validatesOptions
      it "fails when remote cmd behaves unexpectedly" $ withTempDir handlesFailingRemote
      it "provides usage info" $ callCommand "sync-mht --help 2> /dev/null"
      it "provides version info" $ callCommand "sync-mht --version > /dev/null"
      it "ignores boring files" $ runVariations boringTest
      it "syncs large files" $ runVariations addLargeFileTest
      it "syncs random folder hierarchies" $ runVariations syncTest
      it "adds only if -a option is given" $ runVariations addTest
      it "deletes only if --delete option is given" $ runVariations deleteTest
      it "updates only if -u option is given" $ runVariations updateTest

withTempDir :: ((?path :: FilePath) => IO ()) -> IO ()
withTempDir test = withSystemTempDirectory "sync-mht" (\p -> let ?path = p in test)

runVariations :: (Ix a, Bounded a) => ((?path :: FilePath) => a -> IO ()) -> IO ()
runVariations test =
   mapM_ (\x -> withTempDir (test x))
   $ range (minBound,maxBound)

data Variant = Local | ToRemote | FromRemote
   deriving (Eq, Bounded, Ord, Ix)

data Entry =
   File String String | Dir String
   deriving (Eq, Ord, Show)

prepareFolder :: FilePath -> [Entry] -> IO ()
prepareFolder fp = mapM_ go
   where
      go (Dir p) = createDirectory (fp ++ p)
      go (File p q) = writeFile (fp ++ p) q

prepareFolders :: (?path :: FilePath) => [Entry] -> [Entry] -> IO ()
prepareFolders e1 e2 =
   do createDirectory $ ?path </> "source"
      createDirectory $ ?path </> "target"
      prepareFolder (?path </> "source") e1 
      prepareFolder (?path </> "target") e2

readFolder :: FilePath -> IO [Entry]
readFolder fp = liftM sort $ go ""
   where
      go suffix = 
         do contents <- getDirectoryContents (fp ++ suffix)
            result <- mapM (go' suffix) $ filter isRealFile contents
            return $ concat result
      go' suffix entry =
         do let newsuffix = suffix ++ "/" ++ entry
            status <- getFileStatus (fp ++ newsuffix)
            if | isDirectory status -> liftM ((Dir newsuffix):) $ go newsuffix
               | isRegularFile status -> 
                  liftM ((:[]) . (File newsuffix)) $ readFile (fp ++ newsuffix)
               | otherwise -> fail "unexpected file status"

sourceDir :: (?path :: FilePath) => FilePath
sourceDir = ?path </> "source"

targetDir :: (?path :: FilePath) => FilePath
targetDir = ?path </> "target"

expectFolders :: (?path :: FilePath) => [Entry] -> [Entry] -> IO ()
expectFolders e1 e2 = 
   do (readFolder sourceDir) >>= (`shouldBe` (sort e1))
      (readFolder targetDir) >>= (`shouldBe` (sort e2))

validatesOptions :: (?path :: FilePath) => IO ()
validatesOptions =
   do (callCommand $ concat ["sync-mht -s ", sourceDir, " -d ", targetDir, " -r sync-mht", " 2> /dev/null"])
         `shouldThrow` anyException
      (callCommand $ concat ["sync-mht -s remote:", sourceDir, " -d ", targetDir, " 2> /dev/null"])
         `shouldThrow` anyException
      (callCommand $ concat ["sync-mht -s ", sourceDir, " 2> /dev/null"])
         `shouldThrow` anyException

handlesFailingRemote :: (?path :: FilePath) => IO ()
handlesFailingRemote =
   do prepareFolders [] []
      (callCommand $ concat ["sync-mht -s ", sourceDir, " -d remote:", targetDir, " -r /bin/true", " 2> /dev/null"])
         `shouldThrow` anyException

runSyncVariant :: (?path :: FilePath) => Variant -> String -> IO ()
runSyncVariant variant opts =
   callCommand $ concat 
      ["sync-mht -s ", sourceDir', " -d ", targetDir', " ", opts, " ", shellOpt," 2> /dev/null"]
   where
      sourceDir' = (if variant == FromRemote then "remote:" else "") ++ sourceDir
      targetDir' = (if variant == ToRemote then "remote:" else "") ++ targetDir
      shellOpt = (if variant /= Local then "-r sync-mht" else "")

addTest :: (?path :: FilePath) => (Variant, Bool) -> IO ()
addTest (variant, flag) =
   do prepareFolders [File "/added" "a"] []
      runSyncVariant variant (if flag then "-a" else "")
      expectFolders [File "/added" "a"] (if flag then [File "/added" "a"] else [])

addLargeFileTest :: (?path :: FilePath) => Variant -> IO ()
addLargeFileTest variant =
   do prepareFolders [File "/added" largeFile] []
      runSyncVariant variant "-a"
      expectFolders [File "/added" largeFile] [File "/added" largeFile]
   where
      largeFile = show [1..(2^17)]

updateTest :: (?path :: FilePath) => (Variant, Bool) -> IO ()
updateTest (variant, flag) =
   do prepareFolders [File "/updated" "a2"] [File "/updated" "a"]
      runSyncVariant variant (if flag then "-u" else "")
      expectFolders [File "/updated" "a2"] [File "/updated" (if flag then "a2" else "a")]

deleteTest :: (?path :: FilePath) => (Variant, Bool) -> IO ()
deleteTest (variant, flag) =
   do prepareFolders [] [File "/deleted" "a"]
      runSyncVariant variant (if flag then "--delete" else "")
      expectFolders [] (if flag then [] else [File "/deleted" "a"])

syncTest :: (?path :: FilePath) => Variant -> IO ()
syncTest variant =
   do prepareFolders expected actual
      runSyncVariant variant "-a -u --delete"
      expectFolders expected expected
   where
      expected =
         [ Dir "/a"
         , File "/a/keep" "1"
         , Dir "/a/keepemptydir"
         , File "/a/newfile" "2"
         , Dir "/a/newemptydir"
         , Dir "/a/newfulldir"
         , File "/a/newfulldir/a" "3"
         , Dir "/b"
         , File "/b/modfile" "1"
         ]
      actual =
         [ Dir "/a"
         , File "/a/keep" "1"
         , Dir "/a/keepemptydir"
         , File "/a/supfile" "2"
         , Dir "/a/supemptydir"
         , Dir "/a/supfulldir"
         , File "/a/supfulldir/a" "3"
         , Dir "/b"
         , File "/b/modfile" "33"
         ]

boringTest :: (?path :: FilePath) => Variant -> IO ()
boringTest variant =
   do prepareFolders source []
      runSyncVariant variant $ "-a -i x -b " ++ ?path ++ "/source/ignore"
      expectFolders source
         [ File "/d" "d"
         , Dir "/specific"
         , File "/specific/d" "d"
         , File "/ignore" $ unlines ["^specific/file$", "y"]
         ]
   where
      source =
         [ File "/d" "d"
         , Dir "/specific"
         , File "/specific/file" "d"
         , File "/specific/d" "d"
         , File "/ignore" $ unlines ["^specific/file$", "y"]
         , File "/x" "containsx"
         , Dir "/containsy"
         , File "/containsy/a" "a"
         ]

unknownOptionTest :: (?path :: FilePath) => (Variant, Bool) -> IO ()
unknownOptionTest (variant, withDash) =
   runSyncVariant variant (if withDash then "-foo" else "foo") 
   `shouldThrow` anyException

verifyClockDrift :: (?path :: FilePath) => IO ()
verifyClockDrift =
   do let syncOpts =
               defaultSyncOptions
               { so_add = True
               , so_source = Just $ sourceDir
               , so_destination = Just $ "remote:" ++ targetDir
               , so_remote = Just $ "sync-mht"
               }
      prepareFolders [] []
      Just _ <- run $ syncOpts { so_compareClocks = Just (10.0, -100.0) }
      Just _ <- run $ syncOpts { so_compareClocks = Just (10.0, 100.0) }
      Nothing <- run $ syncOpts { so_compareClocks = Just (10.0, 0.0) }
      return ()
 