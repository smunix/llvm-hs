{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Trans.AnyCont where

import Control.Monad.Catch
import Control.Monad.Cont as Cont
import Control.Monad.Fail as Fail
import LLVM.Prelude

newtype AnyContT m a = AnyContT {unAnyContT :: forall r. ContT r m a}

instance Functor (AnyContT m) where
  fmap f (AnyContT p) = AnyContT $ fmap f p

instance Applicative (AnyContT m) where
  pure a = AnyContT $ pure a
  f <*> v = AnyContT $ unAnyContT f <*> unAnyContT v

instance Monad m => Monad (AnyContT m) where
  AnyContT f >>= k = AnyContT $ f >>= (\(AnyContT p) -> p) . k
  return a = AnyContT $ return a

#if !(MIN_VERSION_base(4,13,0))
  fail s = AnyContT (ContT (\_ -> Cont.fail s))
#endif

instance MonadFail m => MonadFail (AnyContT m) where
  fail s = AnyContT (ContT (\_ -> Fail.fail s))

instance MonadIO m => MonadIO (AnyContT m) where
  liftIO = lift . liftIO

instance MonadTrans AnyContT where
  lift ma = AnyContT (lift ma)

instance MonadThrow m => MonadThrow (AnyContT m) where
  throwM = lift . throwM

runAnyContT :: AnyContT m a -> (a -> m r) -> m r
runAnyContT (AnyContT p) = runContT p

anyContT :: (forall r. (a -> m r) -> m r) -> AnyContT m a
anyContT f = AnyContT (ContT f)

withAnyContT :: (forall r. (b -> m r) -> (a -> m r)) -> AnyContT m a -> AnyContT m b
withAnyContT f (AnyContT m) = anyContT $ runContT m . f

mapAnyContT :: (forall r. m r -> m r) -> AnyContT m a -> AnyContT m a
mapAnyContT f (AnyContT m) = anyContT $ f . runContT m
