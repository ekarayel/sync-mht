{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Sync
    ( child
    , local
    , parent
    , openStreams
    , mkChanStreams
    , StreamPair(..)
    , Direction(..)
    ) where

import Control.Concurrent(newChan)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, ask)
import Control.Monad.State
import System.FilePath
import Prelude hiding (lookup)
import System.IO
import System.IO.Streams(InputStream, OutputStream)
import Data.ByteString(ByteString)
import qualified Data.Bytes.Serial as SE
import qualified Data.Bytes.Put as P
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified System.IO.Streams as ST
import qualified System.IO.Streams.Concurrent as ST

import Sync.MerkleTree.Analyse
import Sync.MerkleTree.Channel
import Sync.MerkleTree.Client
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Server
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import Sync.MerkleTree.Util.GetFromInputStream

data StreamPair
    = StreamPair
    { sp_in :: InputStream ByteString
    , sp_out :: OutputStream ByteString
    }

openStreams :: Handle -> Handle -> IO StreamPair
openStreams hIn hOut =
    do inStream <- ST.handleToInputStream hIn
       outStream <- ST.handleToOutputStream hOut
       return $ StreamPair { sp_in = inStream, sp_out = outStream }

mkChanStreams :: IO (InputStream ByteString, OutputStream ByteString)
mkChanStreams =
    do chan <- newChan
       liftM2 (,) (ST.chanToInput chan) (ST.chanToOutput chan)

request :: 
    (MonadReader Channel m, MonadIO m, SE.Serial a, SE.Serial b) => a -> m b
request x = ask >>= (\channel -> liftIO $ makeRequest channel x)

instance Protocol (ReaderT Channel IO) where
    queryHashReq = request . QueryHash
    querySetReq = request . QuerySet
    queryFileReq = request . QueryFile
    queryFileContReq = request . QueryFileCont
    logReq = request . Log
    queryTime = request QueryTime
    terminateReq = request . Terminate

data Direction
    = FromRemote
    | ToRemote

child :: MVar () -> StreamPair -> IO ()
child gotMessage streams =
    do launchMessage <- getFromInputStream (sp_in streams)
       putMVar gotMessage ()
       _ <- serverOrClient (read launchMessage) streams
       return ()

parent ::
    StreamPair
    -> FilePath
    -> FilePath
    -> Direction
    -> ClientServerOptions
    -> IO (Maybe T.Text)
parent streams source destination direction clientServerOpts =
    case direction of
      FromRemote ->
        do respond (sp_out streams) $ show $ mkLaunchMessage Server source
           serverOrClient (mkLaunchMessage Client destination) streams
      ToRemote ->
        do respond (sp_out streams) $ show $ mkLaunchMessage Client destination
           serverOrClient (mkLaunchMessage Server source) streams
    where
      mkLaunchMessage side dir =
          LaunchMessage
          { lm_dir = dir
          , lm_clientServerOptions = clientServerOpts
          , lm_protocolVersion = thisProtocolVersion
          , lm_side = side
          }

respond :: (SE.Serial a) => OutputStream ByteString -> a -> IO ()
respond os = mapM_ (flip ST.write os . Just) . (:[BS.empty]) . P.runPutS . SE.serialize

local :: ClientServerOptions -> FilePath -> FilePath -> IO (Maybe T.Text)
local cs source destination =
    do sourceDir <- liftM (mkTrie 0) $ analyse source (cs_ignore cs)
       destinationDir <- liftM (mkTrie 0) $ analyse destination (cs_ignore cs)
       serverState <- startServerState source sourceDir
       evalStateT (abstractClient cs destination destinationDir) serverState

serverOrClient :: LaunchMessage -> StreamPair -> IO (Maybe T.Text)
serverOrClient lm streams
    | lm_protocolVersion lm == thisProtocolVersion =
        let side =
                case lm_side lm of
                  Server -> server
                  Client -> client (lm_clientServerOptions lm)
        in do entries <- analyse (lm_dir lm) (cs_ignore $ lm_clientServerOptions lm)
              side entries (lm_dir lm) streams
    | otherwise = fail "Incompatible sync-mht versions."

server :: [Entry] -> FilePath -> StreamPair -> IO (Maybe T.Text)
server entries fp streams = (startServerState fp $ mkTrie 0 entries) >>= evalStateT loop
    where
       serverRespond = liftIO . respond (sp_out streams)
       loop =
           do req <- liftIO $ getFromInputStream (sp_in streams)
              case req of
                QueryHash l -> queryHashReq l >>= serverRespond >> loop
                QuerySet l -> querySetReq l >>= serverRespond >> loop
                QueryFile f -> queryFileReq f >>= serverRespond >> loop
                QueryFileCont c -> queryFileContReq c >>= serverRespond >> loop
                Log t -> logReq t >>= serverRespond >> loop
                QueryTime -> queryTime >>= serverRespond >> loop
                Terminate mMsg -> (terminateReq mMsg >>= serverRespond) >> return mMsg

client :: ClientServerOptions -> [Entry] -> FilePath -> StreamPair -> IO (Maybe T.Text)
client cs entries fp streams =
    do channel <- buildChannel (sp_in streams) (sp_out streams) 
       runReaderT (abstractClient cs fp $ mkTrie 0 entries) channel