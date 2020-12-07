{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
module Sync.MerkleTree.Client where

import Control.Monad
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.IO.Class
import Codec.Compression.GZip
import Data.Function
import Data.Monoid(Sum(..))
import Data.Set(Set)
import Data.Time.Clock
import Data.List
import Data.Ratio
import Data.IORef
import Foreign.C.Types
import System.Directory
import System.IO
import System.PosixCompat.Files
import qualified Control.Monad.Parallel as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types

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

logClient :: (Protocol m) => T.Text -> m ()
logClient t =
    do True <- logReq t
       return ()

data SimpleEntry
    = FileSimpleEntry Path
    | DirectorySimpleEntry Path
    deriving (Eq, Ord)

analyseEntries :: Diff Entry -> ([Entry],[Entry],[Entry])
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

data Progress
    = Progress
    { pg_size :: IORef FileSize
    , pg_count :: IORef Int
    , pg_last :: IORef UTCTime
    }

checkClockDiff :: (MonadIO m, Protocol m, ?clientServerOptions :: ClientServerOptions) => m (Maybe T.Text)
checkClockDiff
    | Just (realToFrac -> treshold, realToFrac -> skew) <- compareClocks =
        do t0 <- liftIO getCurrentTime
           t1 <- liftM (addUTCTime skew) queryTime
           t2 <- liftIO getCurrentTime
           case (and [ diffUTCTime t0 t1 < treshold, diffUTCTime t1 t2 < treshold ]) of
             False ->
                 return $ Just $ T.concat
                     [ "Warning: Server and client clocks are apart by at least "
                     , showText treshold, " seconds!"
                     ]
             True ->
                 return Nothing
    | otherwise = return Nothing

abstractClient ::
    (MonadIO m, Protocol m, MonadParallel m, ?clientServerOptions :: ClientServerOptions)
    => FilePath
    -> Trie Entry
    -> m (Maybe T.Text)
abstractClient fp trie =
    do drift <- checkClockDiff
       case drift of
         Nothing -> syncClient fp trie
         Just msg ->
             do True <- terminateReq (Just msg)
                return (Just msg)

syncClient ::
    (MonadIO m, Protocol m, MonadParallel m, ?clientServerOptions :: ClientServerOptions)
    => FilePath
    -> Trie Entry
    -> m (Maybe T.Text)
syncClient fp trie =
    do logClient $ T.concat [ "Hash of destination directory: ", showText $ t_hash trie, "\n" ]
       Diff oent nent <- nodeReq (rootLocation, trie)
       let (delEntries, changedEntries, newEntries) = analyseEntries (Diff oent nent)
       logClient $ T.concat
           [ "Client has ", showText $ length delEntries, " superfluos files of size "
           , dataSizeText delEntries, ", ", showText $ length changedEntries, " changed files of "
           , "size ", dataSizeText changedEntries, " and ", showText $ length newEntries, " "
           , "missing files of size ", dataSizeText newEntries, ".\n"
           ]
       when shouldDelete $
           forM_ (reverse $ sort delEntries) $ \e ->
               case e of
                 FileEntry f -> liftIO $ removeFile $ toFilePath fp $ f_name f
                 DirectoryEntry p -> liftIO $ removeDirectoryRecursive $ toFilePath fp p
       let updateEntries =
               [ e | shouldAdd, e <- newEntries ] ++ [ e | shouldUpdate, e <- changedEntries ]
       progressEntries <- liftIO $ newIORef $ length updateEntries
       progressSize <- liftIO $ newIORef $ dataSize updateEntries
       progressLast <- liftIO $ getCurrentTime >>= newIORef
       let progress =
               Progress
               { pg_size = progressSize
               , pg_count = progressEntries
               , pg_last = progressLast
               }
       mapM_ (syncNewOrChangedEntries progress fp)
           $ groupBy ((==) `on` levelOf) $ sort $ updateEntries
       logClient "Done.                                                                       \n"
       True <- terminateReq Nothing
       return Nothing

_CONCURRENT_FILETRANSFER_SIZE_ :: Int
_CONCURRENT_FILETRANSFER_SIZE_ = 48

splitEvery :: Int -> [a] -> [[a]]
splitEvery n l
    | null l = []
    | (h,t) <- splitAt n l = h:(splitEvery n t)

syncNewOrChangedEntries :: (MonadIO m, Protocol m, MonadParallel m) => Progress -> FilePath -> [Entry] -> m ()
syncNewOrChangedEntries pg fp entries =
    forM_ (splitEvery _CONCURRENT_FILETRANSFER_SIZE_ entries) $ \entryGroup ->
        P.mapM_ (synchronizeNewOrChangedEntry pg fp) entryGroup

showProgess :: (MonadIO m, Protocol m) => Progress -> m ()
showProgess pg =
    do t <- liftIO getCurrentTime
       l <- liftIO $ readIORef (pg_last pg)
       when (diffUTCTime t l > fromRational (1 % 2)) $
           do leftSize <- liftIO $ readIORef (pg_size pg)
              leftCount <- liftIO $ readIORef (pg_count pg)
              logClient $ T.concat
                  [ "Transfering: ", showText $ unFileSize $ leftSize, " bytes and "
                  , showText leftCount, " files left.                  \r"
                  ]
              t2 <- liftIO getCurrentTime
              liftIO $ writeIORef (pg_last pg) t2

synchronizeNewOrChangedEntry :: (MonadIO m, Protocol m) => Progress -> FilePath -> Entry -> m ()
synchronizeNewOrChangedEntry pg fp entry =
    case entry of
      FileEntry f ->
          do firstResult <- queryFileReq (f_name f)
             h <- liftIO $ openFile (toFilePath fp $ f_name f) WriteMode
             let loop result =
                     case result of
                       Final -> return ()
                       ToBeContinued content contHandle ->
                           do let bs = BL.toStrict $ decompress $ BL.fromStrict content
                              liftIO $ BS.hPut h $ bs
                              liftIO $ modifyIORef (pg_size pg)
                                  (subtract $ fromIntegral $ BS.length bs)
                              showProgess pg
                              queryFileContReq contHandle >>= loop
             loop firstResult
             liftIO $ hClose h
             liftIO $ modifyIORef (pg_count pg) (subtract 1)
             let modTime = (CTime $ fromIntegral $ unModTime $ f_modtime f)
             liftIO $ setFileTimes (toFilePath fp $ f_name f) modTime modTime
      DirectoryEntry p ->
          do liftIO $ modifyIORef (pg_count pg) (subtract 1)
             liftIO $ createDirectory $ toFilePath fp p

nodeReq :: (MonadIO m, Protocol m, MonadParallel m) => (TrieLocation, Trie Entry) -> m (Diff Entry)
nodeReq (loc,trie) =
    do fp <- queryHashReq loc
       if | fp == toFingerprint trie -> return mempty
          | Node arr <- t_node trie, NodeType == f_nodeType fp ->
              liftM (foldl1 (<>)) $ P.mapM nodeReq (expand loc arr)
          | otherwise ->
              do s' <- querySetReq loc
                 let s = getAll trie
                 return $ Diff (s `S.difference` s') (s' `S.difference` s)

