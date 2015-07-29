module Sync.MerkleTree.Test where

_FILE_COUNT_ :: Integer

-- | @distinctNames k@ creates k distinct file names
distinctNames :: Integer -> IO [String]
distinctNames k = retry
    where
      isDistinct names = null $ filter hasDuplicate $ group $ sort names
      hasDuplicate = (/= 1) . length
      retry =
          do result <- forM name [1..k]
             if isDistinct result
             then return result
             else retry

-- integration test
testSync oAdd oUpdate oDelete oRemote =
    withSystemTempDirectory $ \testDir ->
        do dirHierarchies <- dirHirarchyPair testDir
           run $
               SyncOptions
               { so_source = Just testDir </> "src"
               , so_destination = Just testDir </> "dest"
               , so_remote :: oRemote
               , so_ignore :: [FilePath]
               , so_add :: oAdd
               , so_update :: oUpdate
               , so_delete :: oDelete
               , so_help = False
               , so_nonOptions = []
               }
           checkDirHierarchy oAdd oUpdate oDelete dirHierarchies

checkDirHierarchy :: Bool -> Bool -> Bool -> Hierarchy -> IO Bool
checkDirHierarchy oAdd uUpdate oDelete hier =
    do

data Kind
    = OnlyA
    | OnlyB
    | Identical
    | Changed

-- Create two similar directories
-- Return all information about them
dirHirarchyPair :: FilePath -> FilePath -> Integer -> IO Hierarchy
dirHirarchyPair root current level =
    do fileCount <- randomRIO (0, _MAX_FILE_COUNT_)
       dirCount <-
           case () of
             () | level < _LEVEL_COUNT_ -> randomRIO (0, _MAX_DIR_COUNT_)
                | otherwise -> 0
       (dirs, files) <- liftM (splitAt dirCount) $ distinctNames (fileCount + dirCount)
       forM files $ \f ->
           do kind <- randomRIO (minBound, maxBound)
              case kind of
                OnlyA -> writeFile (root </> "src" </> current </> f) "SomeContent"
                OnlyB -> writeFile (root </> "dest" </> current </> f) "SomeContent"
                Identical ->
                    do writeFile (root </> "src" </> current </> f) "SomeContent"
                       copyFile (root </> "src" </> current </> f) (root </> "dest" </> current </> f)
                Distinct ->
                    do writeFile (root </> "src" </> current </> f) "SomeContent"
                       writeFile (root </> "dest" </> current </> f) "SomeContent"
       forM dirs $ \d ->
           dirHirarchyPair root (current </> d) (level + 1)


data Hierarchy = Hierarchy


    -- create some files
       -- create some dirs




// -- Create a random directory hierarchy
// -- Create a second similar directory hierarchy
// -- Remember the structure
//


-- An idea would be to have a remote-cmd -- TEST

