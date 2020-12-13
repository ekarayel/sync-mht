module Sync.MerkleTree.Client where

import Codec.Compression.GZip
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import Data.Function
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Monoid (Sum (..))
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time.Clock
import Foreign.C.Types
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import Sync.MerkleTree.Util.Progress
import System.Directory
import System.IO
import System.PosixCompat.Files

data Diff a = Diff (Set a) (Set a)

instance Ord a => Semigroup (Diff a) where
  (<>) (Diff x1 y1) (Diff x2 y2) = Diff (x1 `S.union` x2) (y1 `S.union` y2)

instance Ord a => Monoid (Diff a) where
  mempty = Diff S.empty S.empty

showText :: (Show a) => a -> T.Text
showText = T.pack . show

dataSize :: (F.Foldable f) => f Entry -> FileSize
dataSize s = getSum $ F.foldMap sizeOf s
  where
    sizeOf (FileEntry f) = Sum $ f_size f
    sizeOf (DirectoryEntry _) = Sum $ FileSize 0

dataSizeText :: (F.Foldable f) => f Entry -> T.Text
dataSizeText s = T.concat [showText $ unFileSize $ dataSize s, " bytes"]

logClient :: (Protocol m, MonadFail m) => T.Text -> m ()
logClient t =
  do
    True <- logReq t
    return ()

data SimpleEntry
  = FileSimpleEntry Path
  | DirectorySimpleEntry Path
  deriving (Eq, Ord)

analyseEntries :: Diff Entry -> ([Entry], [Entry], [Entry])
analyseEntries (Diff obsoleteEntries newEntries) =
  (M.elems deleteMap, M.elems changeMap, M.elems newMap)
  where
    deleteMap = M.difference obsMap updMap
    changeMap = M.intersection updMap obsMap
    newMap = M.difference updMap obsMap
    obsMap = M.fromList $ S.toList $ S.map keyValue obsoleteEntries
    updMap = M.fromList $ S.toList $ S.map keyValue newEntries
    keyValue x = (name x, x)
    name (FileEntry f) = FileSimpleEntry $ f_name f
    name (DirectoryEntry f) = DirectorySimpleEntry f

data Cost = Cost
  { c_fileCount :: Int,
    c_dataSize :: FileSize
  }

instance Semigroup Cost where
  (<>) x y =
    Cost
      { c_fileCount = c_fileCount x + c_fileCount y,
        c_dataSize = c_dataSize x + c_dataSize y
      }

instance Monoid Cost where
  mempty = Cost 0 0

checkClockDiff :: (MonadIO m, Protocol m, ?clientServerOptions :: ClientServerOptions) => m (Maybe T.Text)
checkClockDiff
  | Just (realToFrac -> treshold, realToFrac -> skew) <- compareClocks =
    do
      t0 <- liftIO getCurrentTime
      t1 <- liftM (addUTCTime skew) queryTime
      t2 <- liftIO getCurrentTime
      case (and [diffUTCTime t0 t1 < treshold, diffUTCTime t1 t2 < treshold]) of
        False ->
          return $
            Just $
              [i|Warning: Server and client clocks drift by at least #{treshold} seconds!|]
        True ->
          return Nothing
  | otherwise = return Nothing

abstractClient ::
  (MonadIO m, Protocol m, MonadFail m, ?clientServerOptions :: ClientServerOptions) =>
  FilePath ->
  Trie Entry ->
  m (Maybe T.Text)
abstractClient fp trie =
  do
    drift <- checkClockDiff
    case drift of
      Nothing -> syncClient fp trie
      Just msg ->
        do
          True <- terminateReq (Just msg)
          return (Just msg)

syncClient ::
  (MonadIO m, Protocol m, MonadFail m, ?clientServerOptions :: ClientServerOptions) =>
  FilePath ->
  Trie Entry ->
  m (Maybe T.Text)
syncClient fp trie =
  do
    logClient $ [i|Hash of destination directory: #{t_hash trie} \n|]
    Diff oent nent <- nodeReq (rootLocation, trie)
    let (delEntries, changedEntries, newEntries) = analyseEntries (Diff oent nent)
    logClient $
      T.concat
        [ [i|Client has #{length delEntries} superfluos files of size |],
          [i|#{dataSizeText delEntries}, #{length changedEntries} changed files of size |],
          [i|#{dataSizeText changedEntries} and #{length newEntries} missing files of size |],
          [i|#{dataSizeText newEntries}.\n|]
        ]
    when shouldDelete $
      forM_ (reverse $ sort delEntries) $ \e ->
        case e of
          FileEntry f -> liftIO $ removeFile $ toFilePath fp $ f_name f
          DirectoryEntry p -> liftIO $ removeDirectoryRecursive $ toFilePath fp p
    let updateEntries =
          [e | shouldAdd, e <- newEntries] ++ [e | shouldUpdate, e <- changedEntries]
    progressLast <- liftIO $ getCurrentTime >>= newIORef
    runProgress (showProgess progressLast) $
      F.traverse_ (syncNewOrChangedEntries fp) $
        groupBy ((==) `on` levelOf) $ sort $ updateEntries
    logClient "Done.                                                                       \n"
    True <- terminateReq Nothing
    return Nothing

syncNewOrChangedEntries :: (MonadIO m, MonadFail m, Protocol m, HasProgress Cost m) => FilePath -> [Entry] -> m ()
syncNewOrChangedEntries fp entries =
  F.traverse_ (synchronizeNewOrChangedEntry fp) entries

showProgess :: (MonadIO m, MonadFail m, Protocol m) => IORef UTCTime -> ProgressState Cost -> m ()
showProgess progressLast c =
  do
    t <- liftIO getCurrentTime
    l <- liftIO $ readIORef progressLast
    when (diffUTCTime t l > fromRational (1 % 2)) $
      do
        logClient $
          T.concat
            [ [i|Transfering: #{render c_fileCount} files, |],
              [i|#{render (unFileSize . c_dataSize)} bytes.            \r|]
            ]
        t2 <- liftIO getCurrentTime
        liftIO $ writeIORef progressLast t2
  where
    render :: (Show a) => (Cost -> a) -> T.Text
    render f = [i|#{f (ps_completed c)}/#{f (ps_planned c)}|]

synchronizeNewOrChangedEntry :: (MonadIO m, MonadFail m, Protocol m, HasProgress Cost m) => FilePath -> Entry -> m ()
synchronizeNewOrChangedEntry fp entry =
  case entry of
    FileEntry f ->
      do
        progressPlanned $ Cost {c_fileCount = 1, c_dataSize = f_size f}
        firstResult <- queryFileReq (f_name f)
        h <- liftIO $ openFile (toFilePath fp $ f_name f) WriteMode
        let loop result =
              case result of
                Final -> return ()
                ToBeContinued content contHandle ->
                  do
                    let bs = BL.toStrict $ decompress $ BL.fromStrict content
                    liftIO $ BS.hPut h $ bs
                    progressCompleted $ mempty {c_dataSize = fromIntegral $ BS.length bs}
                    queryFileContReq contHandle >>= loop
        loop firstResult
        liftIO $ hClose h
        let modTime = (CTime $ fromIntegral $ unModTime $ f_modtime f)
        liftIO $ setFileTimes (toFilePath fp $ f_name f) modTime modTime
        progressCompleted $ mempty {c_fileCount = 1}
    DirectoryEntry p ->
      do
        progress $ mempty {c_fileCount = 1}
        liftIO $ createDirectory $ toFilePath fp p

nodeReq :: (MonadIO m, Protocol m, MonadFail m) => (TrieLocation, Trie Entry) -> m (Diff Entry)
nodeReq (loc, trie) =
  do
    fp <- queryHashReq loc
    if
        | fp == toFingerprint trie -> return mempty
        | Node arr <- t_node trie,
          NodeType == f_nodeType fp ->
          fmap (foldl1 (<>)) $ traverse nodeReq (expand loc arr)
        | otherwise ->
          do
            s' <- querySetReq loc
            let s = getAll trie
            return $ Diff (s `S.difference` s') (s' `S.difference` s)
