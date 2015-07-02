{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Sync where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString(ByteString)
import Data.Foldable(Foldable)
import Data.Int
import Data.Map(Map)
import Data.Monoid(Monoid, mappend, mempty, Sum(..))
import Data.Ord
import Data.Set(Set)
import Foreign.C.Types
import GHC.Generics
import Prelude hiding (lookup)
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import Sync.MerkleTree.Util.RequestMonad
import System.Directory
import System.IO
import System.IO.Streams(InputStream, OutputStream)
import System.Posix.Files
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Serialize as SE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.IO.Streams as ST

data ContinuationHandle = ContinuationHandle Int
    deriving (Show, Generic)

data Request
    = QuerySet TrieLocation
    | QueryHash TrieLocation
    | Log SerText
    | QueryFile Path
    | QueryFileContinuation ContinuationHandle
    | Terminate
    deriving (Generic, Show)

data QueryFileResponse
    = Final
    | ToBeContinued ByteString ContinuationHandle
    deriving (Generic, Show)

instance SE.Serialize ContinuationHandle
instance SE.Serialize Request
instance SE.Serialize QueryFileResponse

data LogResponse = LogResponse String
    deriving (Generic, Show)

instance SE.Serialize LogResponse

data Diff a = Diff (Set a) (Set a)
    deriving Show

instance Ord a => Monoid (Diff a) where
    mempty = Diff S.empty S.empty
    mappend (Diff x1 y1) (Diff x2 y2) = Diff (x1 `S.union` x2) (y1 `S.union` y2)

logFromClient :: T.Text -> RequestMonad ByteString ()
logFromClient t =
    do LogResponse "Ok" <- request $ Log $ SerText t
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




runSyncClient :: FilePath -> InputStream ByteString -> OutputStream ByteString -> Trie Entry -> IO ()
runSyncClient fp i o trie = runRequestMonad i o $
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
       LogResponse "Ok" <- request $ Terminate
       return ()

respond :: (Show a, SE.Serialize a) => OutputStream ByteString -> a -> IO ()
respond o x =
    do ST.write (Just $ SE.encode x) o
       ST.write (Just "") o


runSyncServer :: FilePath -> InputStream ByteString -> OutputStream ByteString -> Trie Entry -> IO ()
runSyncServer fp i o trie = loop (M.empty, 0)
    where
      addHandle (hs,next) h = withHandle next (Just h) (M.insert next h hs, next+1)
      withMsgHandle ch (handles, j) = withHandle ch (M.lookup ch handles) (handles, j)
      withHandle mh h handles =
          do let Just h' = h
             bs <- BS.hGet h' 4096
             case () of
               () | BS.null bs -> hClose h' >> respond o Final >> loop handles
                  | otherwise -> (respond o $ ToBeContinued bs $ ContinuationHandle mh) >> loop handles
      loop :: (Map Int Handle, Int) -> IO ()
      loop handles =
          do l' <- getFromInputStream i
             case l' of
               QuerySet l -> (respond o $ querySet trie l) >> loop handles
               QueryHash l -> (respond o $ queryHash trie l) >> loop handles
               Log msg -> T.putStrLn (unSerText msg) >> respond o (LogResponse "Ok") >> loop handles
               QueryFileContinuation (ContinuationHandle h) -> withMsgHandle h handles
               QueryFile f -> openFile (toFilePath fp f) ReadMode >>= addHandle handles
               Terminate -> respond o (LogResponse "Ok") >> return ()

nodeReq :: (Ord a, SE.Serialize a, Show a) => (TrieLocation, Trie a) -> RequestMonad ByteString (Diff a)
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










