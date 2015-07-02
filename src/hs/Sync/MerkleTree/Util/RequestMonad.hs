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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Sync.MerkleTree.Util.RequestMonad
    ( RequestMonad
    , request
    , runRequestMonad
    , split
    , getFromInputStream
    ) where

import Data.IORef
import Control.Applicative(Applicative(..))
import Control.Monad(ap,liftM,unless)
import Control.Monad.IO.Class(MonadIO(..))
import System.IO.Streams(InputStream, OutputStream)
import qualified System.IO.Streams as ST
import Control.Concurrent(Chan, writeChan, readChan, newChan, forkIO)
import Data.Serialize(Serialize)
import qualified Data.Serialize as SE
import Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import System.IO(hPutStrLn,stderr)
import Data.Monoid(Monoid, mempty, mappend)

data SplitState f b = forall a. (Monoid a) =>
    SplitState [RequestMonad f a] a (a -> RequestMonad ByteString b)
data RequestState f b = forall a. (Serialize a, Show a) => RequestState f (a -> RequestMonad ByteString b)
data LiftIOState b = forall a. LiftIOState (IO a) (a -> RequestMonad ByteString b)

data RequestMonad f b
    = Split (SplitState f b)
    | Request (RequestState f b)
    | LiftIO (LiftIOState b)
    | Return b
    | Fail String

instance Functor (RequestMonad ByteString) where
    fmap = liftM

instance Applicative (RequestMonad ByteString) where
    pure  = return
    (<*>) = ap

instance Monad (RequestMonad ByteString) where
    return = Return
    fail = Fail
    (>>=) = bindImpl

instance MonadIO (RequestMonad ByteString) where
    liftIO x =  LiftIO $ LiftIOState x Return

bindImpl :: (RequestMonad ByteString a) -> (a -> RequestMonad ByteString b) -> (RequestMonad ByteString b)
bindImpl f g =
    case f of
      Split (SplitState xs z cont) -> Split (SplitState xs z (\t -> bindImpl (cont t) g))
      Request (RequestState r cont) -> Request (RequestState r (\t -> bindImpl (cont t) g))
      LiftIO (LiftIOState op cont) -> LiftIO (LiftIOState op (\t -> bindImpl (cont t) g))
      Return x -> g x
      Fail s -> Fail s

request :: (Serialize a, Serialize b, Show b) => a -> RequestMonad ByteString b
request x = Request $ RequestState (SE.encode x) Return

split :: (Monoid a) => [RequestMonad ByteString a] -> RequestMonad ByteString a
split alts = Split $ SplitState alts mempty Return

queueRequests :: IORef Int -> Chan (Maybe ByteString) -> (RequestMonad ByteString b) -> IO (RequestMonad Int b)
queueRequests idx queue root =
    case root of
      LiftIO (LiftIOState op cont) -> return $ LiftIO (LiftIOState op cont)
      Request (RequestState r c) ->
          do writeChan queue (Just r)
             modifyIORef idx (+1)
             i <- readIORef idx
             return $ Request (RequestState i c)
      Split (SplitState xs z cont) ->
          do xs' <- mapM (queueRequests idx queue) xs
             return $ Split $ SplitState xs' z cont
      Return x -> return $ Return x
      Fail s -> return $ Fail s

runRequestMonad ::
    InputStream ByteString
    -> OutputStream ByteString
    -> RequestMonad ByteString b
    -> IO b
runRequestMonad i o r =
    do q <- newChan
       recvIdx <- newIORef 0
       sendIdx <- newIORef 0
       _ <- forkIO $ writerThread o q
       let loop s =
               do s' <- receiverThread recvIdx sendIdx i q s
                  case s' of
                    Return x -> writeChan q Nothing >> return x
                    Fail err -> fail err
                    _ -> loop s'
       queueRequests sendIdx q r >>= loop

writerThread :: OutputStream ByteString -> Chan (Maybe ByteString) -> IO ()
writerThread o chan = loop
    where
      loop =
          do mBs <- readChan chan
             ST.write mBs o
             ST.write (Just "") o
             maybe (return ()) (const loop) mBs

getFromInputStream :: (Show a, Serialize a) => InputStream ByteString -> IO a
getFromInputStream s = go (SE.Partial $ SE.runGetPartial SE.get)
    where
      go (SE.Fail err bs) =
          hPutStrLn stderr (concat ["X", err]) >> ST.unRead bs s >> fail err
      go (SE.Partial f) =
          do x <- ST.read s
             case x of
               Nothing -> (go $ f BS.empty)
               Just x' | BS.null x' -> go (SE.Partial f)
                       | otherwise -> go (f x')
      go (SE.Done r bs) = ST.unRead bs s >> return r

receiverThread ::
    IORef Int
    -> IORef Int
    -> InputStream ByteString
    -> Chan (Maybe ByteString)
    -> RequestMonad Int b
    -> IO (RequestMonad Int b)
receiverThread recvIdx sendIdx input queue root =
    case root of
      LiftIO (LiftIOState op cont) -> op >>= (queueRequests sendIdx queue . cont)
      Request (RequestState i cont) ->
          do x <- getFromInputStream input
             modifyIORef recvIdx (+1)
             expected <- readIORef recvIdx
             unless (expected == i) $
                 do hPutStrLn stderr $ "Expected " ++ (show i) ++ " but got " ++ show expected
                    fail $ "Expected " ++ show i ++ " but got " ++ show expected
             queueRequests sendIdx queue $ cont x
      Split (SplitState xs z cont) -> loop cont z xs []
      Return x -> return $ Return x
      Fail err -> return $ Fail err
    where
      loop cont z [] [] = queueRequests sendIdx queue $ cont z
      loop cont z [] r = return $ Split $ SplitState (reverse r) z cont
      loop cont z (x:xs) r =
          do x' <- receiverThread recvIdx sendIdx input queue x
             case x' of
               Return x'' -> loop cont (z `mappend` x'') xs r
               Fail s -> return $ Fail s
               other -> loop cont z xs (other:r)


