module Sync.MerkleTree.Test where

import Control.Concurrent
import Control.Monad
import Data.Ix
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Sync.MerkleTree.Analyse
import Sync.MerkleTree.Run
import Sync.MerkleTree.Client
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Posix.Files
import System.Random
import qualified Test.HUnit as H

tests :: H.Test
tests = H.TestList $
    [ testOptions
    , testBigFile
    , testHelp
    , testCmdLine
    , testCmdLineFail
    , testExit
    , testEntry
    , testIgnoreBoring
    , testSync
    ]

testIgnoreBoring :: H.Test
testIgnoreBoring =
    let testCase ign bor = withSystemTempDirectory "sync-mht" $ \fp ->
            do let srcDir = fp </> "src"
                   destDir = fp </> "dest"
               createDirectory srcDir
               createDirectory $ srcDir </> "a"
               createDirectory $ srcDir </> "with-foo"
               createDirectory destDir
               writeFile (fp </> ".boring") $ unlines $ [ "#baz", "", "foo", "^bar" ]
               writeFile (srcDir </> "added.txt") "testA"
               writeFile (srcDir </> "added-bar.txt") "testB"
               writeFile (srcDir </> "baz") "testC"
               writeFile (srcDir </> "bar-ignore.txt") "testD"
               writeFile (srcDir </> "a" </> "bar") "testE"
               writeFile (srcDir </> "some-foo.txt") "testF"
               writeFile (srcDir </> "a" </> "foo") "testG"
               runSync $
                   defaultSyncOptions
                   { so_source = Just $ srcDir
                   , so_destination = Just $ destDir
                   , so_ignore = ign
                   , so_boring = map (const $ fp </> ".boring") bor
                   , so_add = True
                   , so_update = True
                   , so_delete = True
                   }
               readFile (destDir </> "added.txt") >>= ("testA" H.@=?)
               readFile (destDir </> "added-bar.txt") >>= ("testB" H.@=?)
               readFile (destDir </> "baz") >>= ("testC" H.@=?)
               doesFileExist (destDir </> "bar-ignore.txt") >>= (False H.@=?)
               readFile (destDir </> "a" </> "bar") >>= ("testE" H.@=?)
               doesFileExist (destDir </> "some-foo.txt") >>= (False H.@=?)
               doesFileExist (destDir </> "a" </> "foo") >>= (False H.@=?)
               (doesDirectoryExist $ destDir </> "with-foo") >>= (False H.@=?)
    in H.TestList
        [ H.TestLabel "testIgnore" $ H.TestCase $ testCase ["foo","^bar"] []
        , H.TestLabel "testBoring" $ H.TestCase $ testCase [] [()]
        ]

testBigFile :: H.Test
testBigFile = H.TestLabel "testBigFile" $ H.TestCase $
    withSystemTempDirectory "sync-mht" $ \testDir ->
        do let srcDir = testDir </> "src"
               destDir = testDir </> "dest"
               data_ = show [1..(2^17)]
           createDirectory srcDir
           createDirectory destDir
           forM_ [1..400] $ \i -> writeFile (srcDir </> ("new.txt"++show i)) data_
           runSync $
               defaultSyncOptions
               { so_source = Just $ srcDir
               , so_destination = Just $ destDir
               , so_add = True
               }
           dataNew1 <- readFile $ destDir </> "new.txt123"
           dataNew2 <- readFile $ destDir </> "new.txt234"
           dataNew3 <- readFile $ destDir </> "new.txt345"
           data_ H.@=? dataNew1
           data_ H.@=? dataNew2
           data_ H.@=? dataNew3

testCmdLine :: H.Test
testCmdLine = H.TestLabel "testCmdLine" $ H.TestCase $
    withSystemTempDirectory "sync-mht" $ \testDir ->
        do let srcDir = testDir </> "src"
               destDir = testDir </> "dest"
               boringFile = testDir </> ".boring"
               expected = show [1..(2^17)]
           createDirectory srcDir
           createDirectory destDir
           writeFile boringFile "$bar^"
           writeFile (srcDir </> "new.txt") expected
           runMain ["-s",srcDir,"-d",destDir,"-a","-u","--delete","-i","$foo^","-b",boringFile]
           got <- readFile  $ destDir </> "new.txt"
           expected H.@=? got

testCmdLineFail :: H.Test
testCmdLineFail = H.TestLabel "testCmdLineFail" $ H.TestCase $
    withSystemTempDirectory "sync-mht" $ \testDir ->
        do let srcDir = testDir </> "src"
               destDir = testDir </> "dest"
               expected = "test"
           createDirectory srcDir
           createDirectory destDir
           writeFile (srcDir </> "new.txt") expected
           runMain ["-s",srcDir,"-d",destDir,"foo","bar"]
           runMain ["-s",srcDir,"-d","remote:"++destDir,"-r","exit 0"]
           runMain ["-foobar"]
           runMain ["--help"]
           runMain ["-s",srcDir,"-d",destDir]
           (doesFileExist $ destDir </> "new.txt") >>= (False H.@=?)

runMain :: [String] -> IO ()
runMain = main ""

runSync :: SyncOptions -> IO ()
runSync = run ""

testHelp :: H.Test
testHelp = H.TestLabel "testHelp" $ H.TestCase $
    withSystemTempDirectory "sync-mht" $ \testDir ->
        do let srcDir = testDir </> "src"
               destDir = testDir </> "dest"
               data_ = "d"
               syncOpts =
                   defaultSyncOptions
                   { so_add = True
                   , so_source = Just $ srcDir
                   , so_destination = Just $ destDir
                   }
           createDirectory srcDir
           createDirectory destDir
           writeFile (srcDir </> "new.txt") data_
           mapM (runSync) $
               [ syncOpts { so_nonOptions = ["foo", "bar"] }
               , syncOpts { so_source = Just $ "remote:" ++ srcDir }
               , syncOpts { so_source = Nothing }
               , syncOpts { so_destination = Nothing }
               , syncOpts { so_source = Nothing, so_destination = Nothing }
               , defaultSyncOptions
               , syncOpts { so_remote = Just $ RemoteCmd "/bin/true" }
               , syncOpts { so_destination = Just $ "remote:" ++ destDir }
               , syncOpts { so_help = True }
               , syncOpts
                 { so_source = Just $ "remote:" ++ srcDir
                 , so_destination = Just $ "remote:" ++ destDir
                 , so_remote = Just $ RemoteCmd "/bin/true"
                 } ]
           (doesFileExist $ destDir </> "new.txt") >>= (False H.@=?)

testExit :: H.Test
testExit = H.TestLabel "testExit" $ H.TestCase $
    withSystemTempDirectory "sync-mht" $ \testDir ->
        do let srcDir = testDir </> "src"
               destDir = testDir </> "dest"
               data_ = "d"
           createDirectory srcDir
           createDirectory destDir
           writeFile (srcDir </> "new.txt") data_
           res <- (flip catchIOError) (\_ -> return True) $
               do runSync $ defaultSyncOptions
                        { so_source = Just $ "remote:" ++ srcDir
                        , so_destination = Just $ destDir
                        , so_remote = Just $ RemoteCmd "exit"
                        , so_add = True
                        }
                  return False
           True H.@=? res

testOptions :: H.Test
testOptions = H.TestLabel "testOptions" $ H.TestCase $
    let prepare add update delete go =
            withSystemTempDirectory "sync-mht" $ \testDir ->
                do let srcDir = testDir </> "src"
                       destDir = testDir </> "dest"
                   createDirectory srcDir
                   createDirectory destDir
                   writeFile (srcDir </> "same.txt") "test"
                   copyFile (srcDir </> "same.txt") (destDir </> "same.txt")
                   writeFile (srcDir </> "changed.txt") "test"
                   writeFile (destDir </> "changed.txt") "testB"
                   writeFile (srcDir </> "added.txt") "test"
                   writeFile (destDir </> "deleted.txt") "testB"
                   runSync $
                       defaultSyncOptions
                       { so_source = Just $ srcDir
                       , so_destination = Just $ destDir
                       , so_add = add
                       , so_update = update
                       , so_delete = delete
                       }
                   True <- liftM (=="test") $ readFile (srcDir </> "same.txt")
                   True <- liftM (=="test") $ readFile (srcDir </> "changed.txt")
                   True <- liftM (=="test") $ readFile (srcDir </> "added.txt")
                   go destDir
    in do prepare True False False $ \destDir ->
              do readFile (destDir </> "same.txt") >>= ("test" H.@=?)
                 readFile (destDir </> "changed.txt") >>= ("testB" H.@=?)
                 readFile (destDir </> "added.txt") >>= ("test" H.@=?)
                 readFile (destDir </> "deleted.txt") >>= ("testB" H.@=?)
          prepare False True False $ \destDir ->
              do readFile (destDir </> "same.txt") >>= ("test" H.@=?)
                 readFile (destDir </> "changed.txt") >>= ("test" H.@=?)
                 readFile (destDir </> "deleted.txt") >>= ("testB" H.@=?)
          prepare False False True $ \destDir ->
              do readFile (destDir </> "same.txt") >>= ("test" H.@=?)
                 readFile (destDir </> "changed.txt") >>= ("testB" H.@=?)
                 doesFileExist (destDir </> "deleted.txt") >>= (False H.@=?)

testSync :: H.Test
testSync =
    H.TestList
    $ flip map [0,1,2]
    $ \simulate -> H.TestLabel ("testSync"++ show simulate)
    $ H.TestCase
    $ forM_ [1..20] $ \_ ->
        withSystemTempDirectory "sync-mht" $ \testDir ->
            do mkRandomDir 4 [testDir </> "src", testDir </> "src-backup"]
               mkRandomDir 4 [testDir </> "target"]
               let sourcePrefix
                       | simulate == 1 = "remote:"
                       | otherwise = ""
                   targetPrefix
                       | simulate == 2 = "remote:"
                       | otherwise = ""
               let cmd = runSync $
                       defaultSyncOptions
                       { so_source = Just $ (sourcePrefix ++) testDir </> "src"
                       , so_destination = Just $ (targetPrefix ++) testDir </> "target"
                       , so_remote =
                            case simulate of
                              0 -> Nothing
                              _ -> Just Simulate
                       , so_add = True
                       , so_update = True
                       , so_delete = True
                       }
               cmd
               areDirsEqual (testDir </> "src") (testDir </> "target")
               areDirsEqual (testDir </> "src") (testDir </> "src-backup")
               cmd
               areDirsEqual (testDir </> "target") (testDir </> "src-backup")

utcTimeFrom :: Integer -> UTCTime
utcTimeFrom x = UTCTime (fromGregorian 2015 07 29) (fromInteger x)

mkRandomDir :: Integer -> [FilePath] -> IO ()
mkRandomDir md fps =
   do forM fps createDirectory
      names <- distinctNames 6
      forM_ names $ \n ->
          do genDir <- randomIO
             case () of
               () | genDir, md > 0 -> mkRandomDir (md - 1) $ map (</> n) fps
                  | genDir -> return ()
                  | otherwise ->
                      do d <- randomRIO (1,4)
                         forM_ fps $ \fp ->
                             do writeFile (fp </> n) (show d)
                                setModificationTime (fp </> n) (utcTimeFrom d)

areDirsEqual :: FilePath -> FilePath -> IO ()
areDirsEqual fp1 fp2 =
   do files1 <- liftM (sort . filter isRealFile) $ getDirectoryContents fp1
      files2 <- liftM (sort . filter isRealFile) $ getDirectoryContents fp2
      case () of
        () | files1 == files2 -> forM_ files1 $ \f -> areEntriesEqual (fp1 </> f) (fp2 </> f)
           | otherwise -> fail $ "Unequal: " ++ show (fp1, fp2, files1, files2)

areEntriesEqual :: FilePath -> FilePath -> IO ()
areEntriesEqual f1 f2 =
  do s1 <- getFileStatus f1
     s2 <- getFileStatus f2
     case () of
        () | isDirectory s1, isDirectory s2 -> areDirsEqual f1 f2
           | isRegularFile s1, isRegularFile s2 ->
               do c1 <- readFile f1
                  c2 <- readFile f2
                  unless (c1 == c2) $ fail $ "Unequal files: " ++ show (f1, f2, c1, c2)
           | otherwise ->
               fail $ show
                   (f1, f2, isDirectory s1, isDirectory s2, isRegularFile s1, isRegularFile s2)

-- | @distinctNames k@ creates k distinct file names
distinctNames :: Integer -> IO [String]
distinctNames k = retry
    where
      isDistinct names = null $ filter hasDuplicate $ group $ sort names
      hasDuplicate = (/= 1) . length
      mkName _ = liftM (:[]) $ randomRIO ('a','m')
      retry =
          do result <- forM [1..k] mkName
             if isDistinct result
             then return result
             else retry
