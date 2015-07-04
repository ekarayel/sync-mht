{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Client where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable(Foldable)
import Data.Monoid(Monoid, mappend, mempty, Sum(..))
import Data.Set(Set)
import Foreign.C.Types
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Serialize as SE
import qualified Data.Text as T
import Data.Typeable
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import Sync.MerkleTree.Util.RequestMonad
import System.Directory
import System.IO
import System.Posix.Files

data Diff a = Diff (Set a) (Set a)
    deriving Show

instance Ord a => Monoid (Diff a) where
    mempty = Diff S.empty S.empty
    mappend (Diff x1 y1) (Diff x2 y2) = Diff (x1 `S.union` x2) (y1 `S.union` y2)


logFromClient :: T.Text -> RequestMonad ()
logFromClient t =
    do True <- request $ Log $ SerText t
       return ()

showText :: (Show a) => a -> T.Text
showText = T.pack . show

dataSize :: (Foldable f) => f Entry -> FileSize
dataSize s = getSum $ F.foldMap sizeOf s
    where
      sizeOf (FileEntry f) = Sum $ f_size f
      sizeOf (DirectoryEntry _) = Sum $ FileSize 0

dataSizeText :: (Foldable f) => f Entry -> T.Text
dataSizeText s = T.concat [showText $ unFileSize $ dataSize s, " bytes"]

runClient :: RunSide
runClient fp i o trie = runRequestMonad i o $
    do logFromClient $ T.concat [ "Client finished directory traversal: ", showText $ t_hash trie ]
       Diff obsoleteEntries newEntries <- nodeReq (rootLocation, trie)
       logFromClient $ T.concat
           [ "Client has ", showText $ S.size obsoleteEntries, " superfluos files of size "
           , dataSizeText obsoleteEntries, " and ", showText $ S.size newEntries
           , " missing files of size ", dataSizeText newEntries, "."
           ]
       forM_ (S.toDescList obsoleteEntries) $ \e ->
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
                                   res <- request $ QueryFileContinuation ch
                                   loop res
                        res <- request $ QueryFile (f_name f)
                        loop res
                        liftIO $ hClose h
                        let modTime = (CTime $ unModTime $ f_modtime f)
                        liftIO $ setFileTimes (toFilePath fp $ f_name f) modTime modTime
                        bigLoop es (n-1) (s - f_size f)
                 DirectoryEntry p ->
                     (liftIO $ createDirectory $ toFilePath fp p) >> bigLoop es (n-1) s
       bigLoop (S.toAscList newEntries) (S.size newEntries) (dataSize newEntries)
       True <- request $ Terminate
       return ()


nodeReq :: (Ord a, SE.Serialize a, Typeable a) => (TrieLocation, Trie a) -> RequestMonad (Diff a)
nodeReq (loc,trie) =
    do fp <- request $ QueryHash loc
       case () of
         () | fp == toFingerprint trie ->
                return mempty
            | Node arr <- t_node trie, NodeType == f_nodeType fp ->
                split $ map nodeReq (expand loc arr)
            | otherwise ->
                do s' <- request $ QuerySet loc
                   let s = getAll trie
                   return $ Diff (s `S.difference` s') (s' `S.difference` s)



