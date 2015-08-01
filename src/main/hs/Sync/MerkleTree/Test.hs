module Sync.MerkleTree.Test where

import Control.Concurrent
import Control.Monad
import Data.Ix
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Sync.MerkleTree.Analyse
import Sync.MerkleTree.Run
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Temp
import System.Posix.Files
import System.Random
import qualified Distribution.TestSuite as TS

mkTestInstance :: String -> IO TS.Result -> TS.TestInstance
mkTestInstance name run = ti
    where
      ti =
          TS.TestInstance
          { TS.run = liftM TS.Finished (catchIOError run $ return . TS.Fail . show)
          , TS.name = name
          , TS.tags = []
          , TS.options = []
          , TS.setOption = setOpt
          }
      setOpt _ _ = Right ti

tests :: IO [TS.Test]
tests = return $
    [ TS.testGroup "all" $ map TS.Test $
        [ testOptions ]
        ++ testIgnoreBoring
        ++ testSync
    ]

testIgnoreBoring :: [TS.TestInstance]
testIgnoreBoring =
    let testCase ign bor = withSystemTempDirectory "sync-mht" $ \fp ->
            do let srcDir = fp </> "src"
                   destDir = fp </> "dest"
               createDirectory srcDir
               createDirectory $ srcDir </> "a"
               createDirectory destDir
               writeFile (fp </> ".boring") $ unlines $
                   [ "#baz"
                   , ""
                   , "foo"
                   , "^bar"
                   ]
               writeFile (srcDir </> "added.txt") "testA"
               writeFile (srcDir </> "added-bar.txt") "testB"
               writeFile (srcDir </> "baz") "testC"
               writeFile (srcDir </> "bar-ignore.txt") "testD"
               writeFile (srcDir </> "a" </> "bar") "testE"
               writeFile (srcDir </> "some-foo.txt") "testF"
               writeFile (srcDir </> "a" </> "foo") "testG"
               run $
                   SyncOptions
                   { so_source = Just $ srcDir
                   , so_destination = Just $ destDir
                   , so_remote = Nothing
                   , so_ignore = ign
                   , so_boring = map (const $ fp </> ".boring") bor
                   , so_add = True
                   , so_update = True
                   , so_delete = True
                   , so_help = False
                   , so_nonOptions = []
                   }
               True <- liftM (=="testA") $ readFile (destDir </> "added.txt")
               True <- liftM (=="testB") $ readFile (destDir </> "added-bar.txt")
               True <- liftM (=="testC") $ readFile (destDir </> "baz")
               False <- doesFileExist (destDir </> "bar-ignore.txt")
               True <- liftM (=="testE") $ readFile (destDir </> "a" </> "bar")
               False <- doesFileExist (destDir </> "some-foo.txt")
               False <- doesFileExist (destDir </> "a" </> "foo")
               return TS.Pass
    in [ mkTestInstance "testIgnore" $ testCase ["foo","^bar"] []
       , mkTestInstance "testBoring" $ testCase [] [()]
       ]

testOptions :: TS.TestInstance
testOptions = mkTestInstance "testOptions" $
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
                   run $
                       SyncOptions
                       { so_source = Just $ srcDir
                       , so_destination = Just $ destDir
                       , so_remote = Nothing
                       , so_ignore = []
                       , so_boring = []
                       , so_add = add
                       , so_update = update
                       , so_delete = delete
                       , so_help = False
                       , so_nonOptions = []
                       }
                   True <- liftM (=="test") $ readFile (srcDir </> "same.txt")
                   True <- liftM (=="test") $ readFile (srcDir </> "changed.txt")
                   True <- liftM (=="test") $ readFile (srcDir </> "added.txt")
                   go srcDir destDir
    in do prepare True False False $ \srcDir destDir ->
              do True <- liftM (=="test") $ readFile (destDir </> "same.txt")
                 True <- liftM (=="testB") $ readFile (destDir </> "changed.txt")
                 True <- liftM (=="test") $ readFile (destDir </> "added.txt")
                 True <- liftM (=="testB") $ readFile (destDir </> "deleted.txt")
                 return ()
          prepare False True False $ \srcDir destDir ->
              do True <- liftM (=="test") $ readFile (destDir </> "same.txt")
                 True <- liftM (=="test") $ readFile (destDir </> "changed.txt")
                 True <- liftM (=="testB") $ readFile (destDir </> "deleted.txt")
                 return ()
          prepare False False True $ \srcDir destDir ->
              do True <- liftM (=="test") $ readFile (destDir </> "same.txt")
                 True <- liftM (=="testB") $ readFile (destDir </> "changed.txt")
                 False <- doesFileExist (destDir </> "deleted.txt")
                 return ()
          return TS.Pass

testSync :: [TS.TestInstance]
testSync =
    flip map [0,1,2] $ \simulate -> mkTestInstance ("testSync"++ show simulate) $
        do forM [1..10] $ \_ ->
               withSystemTempDirectory "sync-mht" $ \testDir ->
                   do mkRandomDir 3 [testDir </> "src", testDir </> "src-backup"]
                      mkRandomDir 3 [testDir </> "target"]
                      let sourcePrefix
                              | simulate == 1 = "remote:"
                              | otherwise = ""
                          targetPrefix
                              | simulate == 2 = "remote:"
                              | otherwise = ""
                      run $
                          SyncOptions
                          { so_source = Just $ (sourcePrefix ++) testDir </> "src"
                          , so_destination = Just $ (targetPrefix ++) testDir </> "target"
                          , so_remote =
                               case simulate of
                                 0 -> Nothing
                                 _ -> Just Simulate
                          , so_ignore = []
                          , so_boring = []
                          , so_add = True
                          , so_update = True
                          , so_delete = True
                          , so_help = False
                          , so_nonOptions = []
                          }
                      areDirsEqual (testDir </> "src") (testDir </> "target")
                      areDirsEqual (testDir </> "src") (testDir </> "src-backup")
           return TS.Pass

data FileOrDir
    = File
    | Dir
    deriving (Eq, Ix, Enum, Bounded, Ord, Show)

instance Random FileOrDir where
    random = randomR (minBound, maxBound)
    randomR (l, u) g =
       let (i, g) = randomR (fromEnum l, fromEnum u) g
       in (toEnum i, g)

utcTimeFrom :: Integer -> UTCTime
utcTimeFrom x = UTCTime (fromGregorian 2015 07 29) (fromInteger x)

mkRandomDir :: Integer -> [FilePath] -> IO ()
mkRandomDir md fps =
   do forM fps createDirectory
      names <- distinctNames 6
      forM_ names $ \n ->
          do m <- randomRIO (0,1)
             case toEnum m of
               Dir | md > 0 -> mkRandomDir (md - 1) $ map (</> n) fps
                   | otherwise -> return ()
               File ->
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
                  unless (c1 == c2) $fail $ "Unequal files: " ++ show (f1, f2, c1, c2)
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
