-- |
-- Description : Provides support for request reply communication over a channel.
--
-- The RPCClient monad supports a call function, which will send the serialized
-- version of the provided element over the channel and wait for the response.
-- The monad internally is implemented in a way that it optimizes the evaluation
-- to keep the send channel full, e.g., when evaluating:
--
-- @
--   liftA2 (combine) (call q1) (call q2)
-- @
--
-- the runner will evaluate it like
--
-- @
--     do send q1
--        send q2
--        r1 <- receive
--        r2 <- receive
--        return $ combine r1 r2
-- @
--
-- Note that the runner assumes FIFO semantics between the channels.
module Sync.MerkleTree.Util.RPCClient
  ( call,
    runRPCClient,
    RPCClient,
    HasCall,
  )
where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Bytes.Serial (Serial)
import Sync.MerkleTree.Util.Communication
import System.IO.Streams (InputStream, OutputStream)

type RPCClient = ClientMonad CallExp

data CallExp b = forall a. (Serial a, Serial b) => CallExp a -- TODO: Rename these

data RecvExp b = (Serial b) => RecvExp

data VoidExp b

data ClientMonad w b where
  Pure :: b -> ClientMonad w b
  Bind :: ClientMonad w a -> (a -> ClientMonad CallExp b) -> ClientMonad w b
  App :: ClientMonad w (a -> b) -> (ClientMonad w a) -> ClientMonad w b
  Call :: !(w b) -> ClientMonad w b
  Lift :: IO b -> ClientMonad w b

instance Functor (ClientMonad CallExp) where
  fmap = App . Pure

instance Applicative (ClientMonad CallExp) where
  pure = Pure
  (<*>) = App

instance Monad (ClientMonad CallExp) where
  return = pure
  (>>=) = Bind

instance MonadIO (ClientMonad CallExp) where
  liftIO = Lift

instance MonadFail (ClientMonad CallExp) where
  fail = liftIO . fail

class HasCall m where
  call :: (Serial a, Serial b) => a -> m b

instance HasCall (ClientMonad CallExp) where
  call = Call . CallExp

preprocess :: (forall b. v b -> IO (ClientMonad w b)) -> ClientMonad v a -> IO (ClientMonad w a)
preprocess f (Bind x y) = do
  x' <- preprocess f x
  return $ Bind x' y
preprocess f (App x y) = do
  x' <- preprocess f x
  y' <- preprocess f y
  return $ App x' y'
preprocess f (Call x) = f x
preprocess _ (Pure x) = return $ Pure x
preprocess _ (Lift x) = return $ Lift x

runRPCClient :: InputStream ByteString -> OutputStream ByteString -> ClientMonad CallExp a -> IO a
runRPCClient input output = start
  where
    start :: ClientMonad CallExp a -> IO a
    start x = preprocess sendRequests x >>= preprocess receiveResponses >>= go

    sendRequests :: CallExp a -> IO (ClientMonad RecvExp a)
    sendRequests (CallExp x) = send output x >> return (Call (RecvExp))

    receiveResponses :: RecvExp a -> IO (ClientMonad VoidExp a)
    receiveResponses (RecvExp) = receive input >>= (return . Pure)

    go :: ClientMonad VoidExp a -> IO a
    go (Pure x) = pure x
    go (Lift x) = x
    go (Bind x y) = do
      x' <- go x
      start (y x')
    go (App x y) = do
      x' <- go x
      y' <- go y
      return $ x' y'
