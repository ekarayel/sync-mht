-- | RequestMonad
--
-- This monad is used to increase the speed of communication between two processes - if there is
-- latency. It works by using the non-deterministic part of the communication protocol to send
-- multiple requests to the output-channel, before processing the responses from the input-channel.
--
-- An example:
-- @
--     foo = split [bar, baz]
--     bar = do x <- request (GetSumOf 1 2)
--              liftM Sum request (GetSumOf x 3)
--     baz = liftM Sum request (GetSumOf 4 5)
-- @
--
-- In this case
-- @
--    runRequestMonad inputHandle outputHandle foo
-- @
-- will send both messages GetSumOf 1 2, GetSumOf 4 5, without having to wait for the repsonse to
-- the first request. The last request GetSumOf 3 3 will be send after the response for the first
-- message has arrived.
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Util.RequestMonad
    ( RequestMonad
    , request
    , runRequestMonad
    , splitRequests
    , getFromInputStream
    ) where

import Control.Applicative(Applicative(..))
import Control.Concurrent(Chan, writeChan, readChan, newChan, forkIO)
import Control.Monad(ap,liftM,unless)
import Control.Monad.IO.Class(MonadIO(..))
import Data.ByteString(ByteString)
import Data.IORef(IORef,newIORef,modifyIORef,readIORef)
import Data.Monoid(Monoid, mempty, mappend)
import Data.Serialize(Serialize)
import Data.Typeable(Typeable, Proxy(..), typeRep)
import System.IO(hPutStrLn,stderr)
import System.IO.Streams(InputStream, OutputStream)
import qualified Data.ByteString as BS
import qualified Data.Serialize as SE
import qualified System.IO.Streams as ST

data SplitState f b = forall a. (Monoid a) => SplitState [RequestMonadT f a] a (a -> RequestMonad b)
data RequestState f b = forall a. (Serialize a, Typeable a) => RequestState f (a -> RequestMonad b)
data LiftIOState b = forall a. LiftIOState (IO a) (a -> RequestMonad b)

type RequestMonad = RequestMonadT ByteString

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
    fail = Fail
    (>>=) = bindImpl

instance MonadIO (RequestMonadT ByteString) where
    liftIO x =  LiftIO $ LiftIOState x Return

bindImpl :: (RequestMonad a) -> (a -> RequestMonad b) -> (RequestMonad b)
bindImpl f g =
    case f of
      Split (SplitState xs z cont) -> Split (SplitState xs z (\t -> bindImpl (cont t) g))
      Request (RequestState r cont) -> Request (RequestState r (\t -> bindImpl (cont t) g))
      LiftIO (LiftIOState op cont) -> LiftIO (LiftIOState op (\t -> bindImpl (cont t) g))
      Return x -> g x
      Fail s -> Fail s

request :: (Serialize a, Serialize b, Typeable b) => a -> RequestMonad b
request x = Request $ RequestState (SE.encode x) Return

splitRequests :: (Monoid a) => [RequestMonad a] -> RequestMonad a
splitRequests alts = Split $ SplitState alts mempty Return

data SendQueue
    = SendQueue
    { sq_chan :: Chan (Maybe ByteString)
    , sq_sendIndex :: IORef Int
    }

queueRequests :: SendQueue -> (RequestMonad b) -> IO (RequestMonadT Int b)
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
       queueRequests sq startMonad >>= loop

writerThread :: OutputStream ByteString -> Chan (Maybe ByteString) -> IO ()
writerThread os chan = loop
    where
      loop =
          do mBs <- readChan chan
             ST.write mBs os
             ST.write (Just "") os
             maybe (return ()) (const loop) mBs

getFromInputStream :: (Serialize a) => InputStream ByteString -> IO a
getFromInputStream s = go (SE.Partial $ SE.runGetPartial SE.get)
    where
      go (SE.Fail err bs) = ST.unRead bs s >> fail err
      go (SE.Partial f) =
          do x <- ST.read s
             case x of
               Nothing -> (go $ f BS.empty)
               Just x' | BS.null x' -> go (SE.Partial f)
                       | otherwise -> go (f x')
      go (SE.Done r bs) = ST.unRead bs s >> return r

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
             unless (expected == i) $
                 do hPutStrLn stderr $ "Expected " ++ (show i) ++ " but got " ++ show expected
                    fail $ "Expected " ++ show i ++ " but got " ++ show expected
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


