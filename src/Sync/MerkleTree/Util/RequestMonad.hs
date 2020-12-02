-- | This monad is used to increase the speed of communication between two processes - if there is
-- latency. It works by using the non-deterministic part of the communication protocol to send
-- multiple requests to the output-channel, before processing the responses from the input-channel.
--
-- Considering the example
--
-- @
-- foo = splitRequests [bar, baz]
-- bar = do x <- request (GetSumOf 1 2)
--          liftM Sum request (GetSumOf x 3)
-- baz = liftM Sum request (GetSumOf 4 5)
-- @
--
-- running @foo@ in the @RequestMonad@:
--
-- @
-- runRequestMonad inputHandle outputHandle foo
-- @
--
-- will send both messages @GetSumOf 1 2@, @GetSumOf 4 5@, without having to wait for the repsonse
-- to the first request. The last request @GetSumOf 3 3@ will be send after the response for the
-- first message has arrived.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sync.MerkleTree.Util.RequestMonad
    ( RequestMonad
    , request
    , runRequestMonad
    , splitRequests
    ) where

import Control.Concurrent(Chan, writeChan, readChan, newChan, forkIO)
import Control.Monad(ap,liftM,unless)
import Control.Monad.IO.Class(MonadIO(..))
import Data.ByteString(ByteString)
import Data.IORef(IORef,newIORef,modifyIORef,readIORef)
import System.IO.Streams(InputStream, OutputStream)
import Sync.MerkleTree.Util.GetFromInputStream
import qualified Data.Bytes.Serial as SE
import qualified Data.Bytes.Put as P
import qualified System.IO.Streams as ST

data SplitState f b =
    forall a. (Monoid a) => SplitState [RequestMonadT f a] a (a -> RequestMonadT ByteString b)
data RequestState f b = forall a. (SE.Serial a) => RequestState f (a -> RequestMonadT ByteString b)
data LiftIOState b = forall a. LiftIOState (IO a) (a -> RequestMonadT ByteString b)

newtype RequestMonad b = RequestMonad { unReqMonad :: RequestMonadT ByteString b }
    deriving (Monad, MonadFail, Functor, Applicative, MonadIO)

data RequestMonadT f b
    = Split (SplitState f b)
    | Request (RequestState f b)
    | LiftIO (LiftIOState b)
    | Return b
    | Fail String

instance Functor (RequestMonadT ByteString) where
    fmap = liftM

instance Applicative (RequestMonadT ByteString) where
    pure  = return
    (<*>) = ap

instance Monad (RequestMonadT ByteString) where
    return = Return
    (>>=) = bindImpl

instance MonadFail (RequestMonadT ByteString) where
    fail = Fail

instance MonadIO (RequestMonadT ByteString) where
    liftIO x =  LiftIO $ LiftIOState x Return

bindImpl ::
    (RequestMonadT ByteString a)
    -> (a -> RequestMonadT ByteString b)
    -> (RequestMonadT ByteString b)
bindImpl f g =
    case f of
      Split (SplitState xs z cont) -> Split (SplitState xs z (\t -> bindImpl (cont t) g))
      Request (RequestState r cont) -> Request (RequestState r (\t -> bindImpl (cont t) g))
      LiftIO (LiftIOState op cont) -> LiftIO (LiftIOState op (\t -> bindImpl (cont t) g))
      Return x -> g x
      Fail s -> Fail s

request :: (SE.Serial a, SE.Serial b) => a -> RequestMonad b
request x = RequestMonad $ Request $ RequestState (P.runPutS $ SE.serialize x) Return

-- | Combine results in the monad non-deterministically
-- (it is required that the monoid is commutative)
splitRequests :: (Monoid a) => [RequestMonad a] -> RequestMonad a
splitRequests alts = RequestMonad $ Split $ SplitState (map unReqMonad alts) mempty Return

data SendQueue
    = SendQueue
    { sq_chan :: Chan (Maybe ByteString)
    , sq_sendIndex :: IORef Int
    }

queueRequests :: SendQueue -> (RequestMonadT ByteString b) -> IO (RequestMonadT Int b)
queueRequests sq root =
    case root of
      LiftIO (LiftIOState op cont) -> return $ LiftIO (LiftIOState op cont)
      Request (RequestState r c) ->
          do writeChan (sq_chan sq) (Just r)
             modifyIORef (sq_sendIndex sq) (+1)
             i <- readIORef (sq_sendIndex sq)
             return $ Request (RequestState i c)
      Split (SplitState xs z cont) ->
          do xs' <- mapM (queueRequests sq) xs
             return $ Split $ SplitState xs' z cont
      Return x -> return $ Return x
      Fail s -> return $ Fail s

-- | Run the provided request monad using the given communication channels
runRequestMonad ::
    InputStream ByteString
    -> OutputStream ByteString
    -> RequestMonad b
    -> IO b
runRequestMonad is os startMonad =
    do sendChan <- newChan
       recvIdx <- newIORef 0
       sendIdx <- newIORef 0
       _ <- forkIO $ writerThread os sendChan
       let sq = SendQueue { sq_chan = sendChan, sq_sendIndex = sendIdx }
           loop monad =
               do monad' <- receiverThread recvIdx sq is monad
                  case monad' of
                    Return x -> writeChan sendChan Nothing >> return x
                    Fail err -> fail err
                    _ -> loop monad'
       queueRequests sq (unReqMonad startMonad) >>= loop

writerThread :: OutputStream ByteString -> Chan (Maybe ByteString) -> IO ()
writerThread os chan = loop
    where
      loop =
          do mBs <- readChan chan
             ST.write mBs os
             ST.write (Just "") os
             maybe (return ()) (const loop) mBs

receiverThread ::
    IORef Int
    -> SendQueue
    -> InputStream ByteString
    -> RequestMonadT Int b
    -> IO (RequestMonadT Int b)
receiverThread recvIdx sq input root =
    case root of
      LiftIO (LiftIOState op cont) -> op >>= (queueRequests sq . cont)
      Request (RequestState i cont) ->
          do x <- getFromInputStream input
             modifyIORef recvIdx (+1)
             expected <- readIORef recvIdx
             unless (expected == i) $ fail ("Expected " ++ (show i) ++ " but got " ++ show expected)
             queueRequests sq $ cont x
      Split (SplitState xs z cont) -> loop cont z xs []
      Return x -> return $ Return x
      Fail err -> return $ Fail err
    where
      loop cont z [] [] = queueRequests sq $ cont z
      loop cont z [] r = return $ Split $ SplitState (reverse r) z cont
      loop cont z (x:xs) r =
          do x' <- receiverThread recvIdx sq input x
             case x' of
               Return x'' -> loop cont (z `mappend` x'') xs r
               Fail s -> return $ Fail s
               other -> loop cont z xs (other:r)
