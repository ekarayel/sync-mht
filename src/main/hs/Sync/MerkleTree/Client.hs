{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Sync.MerkleTree.Client where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable(Foldable)
import Data.Monoid(Monoid, mappend, mempty, Sum(..))
import Data.Set(Set)
import Data.List
import Foreign.C.Types
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import System.Directory
import System.IO
import System.Posix.Files

data Diff a = Diff (Set a) (Set a)
    deriving Show

instance Ord a => Monoid (Diff a) where
    mempty = Diff S.empty S.empty
    mappend (Diff x1 y1) (Diff x2 y2) = Diff (x1 `S.union` x2) (y1 `S.union` y2)

showText :: (Show a) => a -> T.Text
showText = T.pack . show

dataSize :: (Foldable f) => f Entry -> FileSize
dataSize s = getSum $ F.foldMap sizeOf s
    where
      sizeOf (FileEntry f) = Sum $ f_size f
      sizeOf (DirectoryEntry _) = Sum $ FileSize 0

dataSizeText :: (Foldable f) => f Entry -> T.Text
dataSizeText s = T.concat [showText $ unFileSize $ dataSize s, " bytes"]

class (ProtocolM m) => (ClientMonad m) where
    split :: (Monoid a) => [m a] -> m a

logClient :: (ClientMonad m) => T.Text -> m ()
logClient t =
    do True <- logReq $ SerText t
       return ()

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
      name (FileEntry f) = f_name f
      name (DirectoryEntry f) = f

abstractClient :: (ClientMonad m) => ClientServerOptions -> FilePath -> Trie Entry -> m ()
abstractClient cs fp trie =
    do logClient $ T.concat [ "Client finished directory traversal: ", showText $ t_hash trie ]
       Diff oent nent <- nodeReq (rootLocation, trie)
       let (delEntries, changedEntries, newEntries) = analyseEntries (Diff oent nent)
       logClient $ T.concat
           [ "Client has ", showText $ length delEntries, " superfluos files of size "
           , dataSizeText delEntries, ", ", showText $ length changedEntries, " changed files "
           , "of size ", dataSizeText changedEntries, " and ", showText $ length newEntries
           , " missing files of size ", dataSizeText newEntries, "."
           ]
       when (cs_delete cs) $
           forM_ (reverse $ sort delEntries) $ \e ->
               case e of
                 FileEntry f -> liftIO $ removeFile $ toFilePath fp $ f_name f
                 DirectoryEntry p -> liftIO $ removeDirectory $ toFilePath fp p
       let bigLoop [] _ _ = return ()
           bigLoop (e:es) n s =
               case e of
                 FileEntry f ->
                     do h <- liftIO $ openFile (toFilePath fp $ f_name f) WriteMode
                        let loop Final = return ()
                            loop (ToBeContinued bs ch) =
                                do liftIO $ BS.hPut h bs
                                   res <- queryFileContReq ch
                                   loop res
                        res <- queryFileReq (f_name f)
                        loop res
                        liftIO $ hClose h
                        let modTime = (CTime $ unModTime $ f_modtime f)
                        liftIO $ setFileTimes (toFilePath fp $ f_name f) modTime modTime
                        bigLoop es (n-1) (s - f_size f)
                 DirectoryEntry p ->
                     (liftIO $ createDirectory $ toFilePath fp p) >> bigLoop es (n-1) s
       let copyEnt = [ e | cs_add cs, e <- newEntries ] ++ [ e | cs_update cs, e <- changedEntries ]
       bigLoop (sort copyEnt) (length copyEnt) (dataSize newEntries)
       True <- terminateReq
       return ()

nodeReq :: (ClientMonad m) => (TrieLocation, Trie Entry) -> m (Diff Entry)
nodeReq (loc,trie) =
    do fp <- queryHashReq loc
       case () of
         () | fp == toFingerprint trie ->
                return mempty
            | Node arr <- t_node trie, NodeType == f_nodeType fp ->
                split $ map nodeReq (expand loc arr)
            | otherwise ->
                do s' <- querySetReq loc
                   let s = getAll trie
                   return $ Diff (s `S.difference` s') (s' `S.difference` s)



