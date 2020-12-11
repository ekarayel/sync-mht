{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Sync
    ( child
    , local
    , parent
    , openStreams
    , StreamPair(..)
    , Direction(..)
    ) where

import Control.Concurrent.MVar
import Control.Monad
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

import Sync.MerkleTree.Analyse
import Sync.MerkleTree.Client
import Sync.MerkleTree.CommTypes
import Sync.MerkleTree.Server
import Sync.MerkleTree.Trie
import Sync.MerkleTree.Types
import Sync.MerkleTree.Util.Communication
import Sync.MerkleTree.Util.RPCClient

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

instance Protocol RPCClient where
    queryHashReq = call . QueryHash
    querySetReq = call . QuerySet
    queryFileReq = call . QueryFile
    queryFileContReq = call . QueryFileCont
    logReq = call . Log
    queryTime = call QueryTime
    terminateReq = call . Terminate

data Direction
    = FromRemote
    | ToRemote

child :: MVar () -> StreamPair -> IO ()
child gotMessage streams =
    do launchMessage <- receive (sp_in streams)
       putMVar gotMessage ()
       _ <- serverOrClient (read launchMessage) streams
       return ()

parent ::
    (?clientServerOptions :: ClientServerOptions) 
    => StreamPair
    -> FilePath
    -> FilePath
    -> Direction
    -> IO (Maybe T.Text)
parent streams source destination direction =
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
          , lm_clientServerOptions = ?clientServerOptions
          , lm_protocolVersion = thisProtocolVersion
          , lm_side = side
          }

respond :: (SE.Serial a) => OutputStream ByteString -> a -> IO ()
respond os = mapM_ (flip ST.write os . Just) . (:[BS.empty]) . P.runPutS . SE.serialize

local :: (?clientServerOptions :: ClientServerOptions) => FilePath -> FilePath -> IO (Maybe T.Text)
local source destination =
    do sourceDir <- liftM (mkTrie 0) $ analyse source ignorePaths
       destinationDir <- liftM (mkTrie 0) $ analyse destination ignorePaths
       serverState <- startServerState source sourceDir
       evalStateT (syncClient destination destinationDir) serverState

serverOrClient :: LaunchMessage -> StreamPair -> IO (Maybe T.Text)
serverOrClient lm streams
    | lm_protocolVersion lm == thisProtocolVersion =
        let side =
                case lm_side lm of
                  Server -> server
                  Client -> client 
        in do entries <- analyse (lm_dir lm) ignorePaths
              side entries (lm_dir lm) streams
    | otherwise = fail "Incompatible sync-mht versions."
    where
        ?clientServerOptions = lm_clientServerOptions lm

server :: [Entry] -> FilePath -> StreamPair -> IO (Maybe T.Text)
server entries fp streams = (startServerState fp $ mkTrie 0 entries) >>= evalStateT loop
    where
       serverRespond = liftIO . respond (sp_out streams)
       loop =
           do req <- liftIO $ receive (sp_in streams)
              case req of
                QueryHash l -> queryHashReq l >>= serverRespond >> loop
                QuerySet l -> querySetReq l >>= serverRespond >> loop
                QueryFile f -> queryFileReq f >>= serverRespond >> loop
                QueryFileCont c -> queryFileContReq c >>= serverRespond >> loop
                Log t -> logReq t >> serverRespond True >> loop
                QueryTime -> queryTime >>= serverRespond >> loop
                Terminate mMsg -> terminateReq mMsg >> serverRespond True >> return mMsg

client :: (?clientServerOptions :: ClientServerOptions) => [Entry] -> FilePath -> StreamPair -> IO (Maybe T.Text)
client entries fp streams =
       runRPCClient (sp_in streams) (sp_out streams) (abstractClient fp $ mkTrie 0 entries)