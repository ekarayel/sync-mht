{-|
Description : Support for concurrent access to a bidirection channel.

Enables using the same pair of an input and output stream within concurrent
code, by a making sure that each query message is delivered un-interleaved and
blocking the requesting thread until its response has been received.
However crucially, once a request has been sent another thread can send a 
second request, without the first requests' response having been received yet.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sync.MerkleTree.Channel
    ( Channel
    , buildChannel
    , makeRequest
    ) where

import Control.Concurrent.STM
import Data.ByteString(ByteString)
import Data.IORef(IORef,newIORef,modifyIORef,readIORef)
import System.IO.Streams(InputStream, OutputStream)
import Sync.MerkleTree.Util.GetFromInputStream
import qualified Data.Bytes.Serial as SE
import qualified Data.Bytes.Put as P
import qualified System.IO.Streams as ST
import qualified Control.Concurrent.Lock as L

data Channel =
    Channel
    { c_lock :: L.Lock
    , c_in :: InputStream ByteString
    , c_out :: OutputStream ByteString
    , c_countIn :: IORef Integer
    , c_countOut :: TVar Integer 
    }

buildChannel :: InputStream ByteString -> OutputStream ByteString -> IO Channel
buildChannel is os =
    do lock <- L.new
       countOut <- newTVarIO 0
       countIn <- newIORef 0
       return 
         Channel 
         { c_lock = lock
         , c_countIn = countIn
         , c_countOut = countOut
         , c_in = is
         , c_out = os
         }

makeRequest :: (SE.Serial a, SE.Serial b) => Channel -> a -> IO b
makeRequest channel req =
    do L.acquire $ c_lock channel
       reservedIndex <- readIORef $ c_countIn channel
       modifyIORef (c_countIn channel) (+1)
       ST.write (Just $ P.runPutS $ SE.serialize req) (c_out channel)
       ST.write (Just "") (c_out channel) -- flush underlying handle
       L.release (c_lock channel)
       atomically $ 
          do x <- readTVar (c_countOut channel) 
             check (x==reservedIndex) 
       result <- getFromInputStream (c_in channel)
       atomically $ modifyTVar (c_countOut channel) (+1)
       return result
