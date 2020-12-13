{-|
Description : Provides support for estimating the remaining and already completed 
steps during the run of a computation.

The estimates are derived using look ahead from the applicative part of the 
monad. For example while executing:

@
  (doWork *> progress 1 *> doWork *> progress 1)
@

The runner will report at the beginning that 0 of 2 steps have been completed,
and then update to 1 of 2 steps and finally to 2 of 2 steps. The advantage is
that there is no need to precomute the total number of steps.

Instead of integer's it is possible to describe progress in an arbitrary
monoid.

If the computation cannot be expressed in applicative fashion it is also
possible to use pairs of progressPlanned and progressCompleted.

The estimator can also handle situations when the planned cost may increase
during the run of the computation.
-}
module Sync.MerkleTree.Util.Progress
    ( progress
    , progressPlanned
    , progressCompleted
    , runProgress
    , ProgressMonad
    , HasProgress
    , ProgressState(..)
    ) where

import Control.Monad.State

data ProgressState w =
    ProgressState
    { ps_planned :: w
    , ps_completed :: w
    }
    deriving (Eq, Show)

instance Semigroup w => Semigroup (ProgressState w) where
    x <> y = 
        ProgressState
        { ps_planned = ps_planned x <> ps_planned y
        , ps_completed = ps_completed x <> ps_completed y
        }

instance Monoid w => Monoid (ProgressState w) where
    mempty = ProgressState mempty mempty

data ProgressMonad w m b where
    Pure :: b -> ProgressMonad w m b
    Bind :: ProgressMonad w m a -> (a -> ProgressMonad w m b) -> ProgressMonad w m b
    App :: ProgressMonad w m (a -> b) -> (ProgressMonad w m a) -> ProgressMonad w m b
    Progress :: ProgressState w -> ProgressMonad w m ()
    Lift :: m b -> ProgressMonad w m b

instance Functor (ProgressMonad w m) where
    fmap = App . Pure

instance Applicative (ProgressMonad w m) where
    pure = Pure
    (<*>) = App

instance Monad (ProgressMonad w m) where
    return = pure
    (>>=) = Bind

instance MonadTrans (ProgressMonad w) where
    lift = Lift

instance (MonadIO m) => MonadIO (ProgressMonad w m) where
    liftIO = Lift . liftIO

instance (MonadFail m) => MonadFail (ProgressMonad w m) where
    fail = Lift . fail

class HasProgress w m where
    progressMethod :: ProgressState w -> m ()

instance HasProgress w (ProgressMonad w m) where
    progressMethod = Progress

progress :: (HasProgress w m) => w -> m ()
progress x = progressMethod $ ProgressState { ps_planned = x, ps_completed = x }

progressPlanned :: (Monoid w, HasProgress w m) => w -> m ()
progressPlanned x = progressMethod $ mempty { ps_planned = x }

progressCompleted :: (Monoid w, HasProgress w m) => w -> m ()
progressCompleted x = progressMethod $ mempty {  ps_completed = x }

estimate :: (Monoid w) => ProgressMonad w m b -> w
estimate (Bind x _) = estimate x
estimate (App x y) = estimate x <> estimate y
estimate (Progress x) = ps_planned x
estimate _ = mempty

runProgress :: (Monoid w, Monad m) => (ProgressState w -> m ()) -> ProgressMonad w m a -> m a
runProgress (report :: ProgressState w -> m ()) root = evalStateT (start root) mempty
    where
        start :: (ProgressMonad w m a) -> StateT (ProgressState w) m a
        start x = do
            let planned = mempty { ps_planned = estimate x }
            modify (<> planned)
            get >>= lift . report
            go x

        go :: (ProgressMonad w m a) -> StateT (ProgressState w) m a
        go (Pure x) = pure x
        go (Lift x) = lift x
        go (Bind x y) = do 
            x' <- go x
            start (y x')
        go (Progress x) = do
            modify (<> x { ps_planned = mempty })
            get >>= lift . report
        go (App x y) = do
            x' <- go x
            y' <- go y
            return $ x' y'