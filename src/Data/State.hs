{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
module Data.State where

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad

import Data.Function
import Data.Identity
import Data.Kind
import Data.Tuple

--newtype State s a = State { runState :: s -> (a, s) }
newtype StateT s (m :: Type -> Type) a = StateT { unStateT :: s -> m (a, s) }

type State s a = StateT s Identity a

stateT :: Monad m => (s -> (a, s)) -> StateT s m a
stateT f = StateT $ pure . f

runStateT :: StateT s m a -> s -> m (a, s)
runStateT (StateT state) s = state s

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

---------------------------
--- TYPECLASS INSTANCES ---
---------------------------

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \s ->  (\(a',s') -> (f a', s')) <$> runStateT m s

instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a  = StateT $ \s -> pure (a, s)
  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (<*>) (StateT mf) (StateT ma) = StateT $ \s ->
    mf s >>= \(f, s') -> ma s' >>= \(a, s'') -> pure (f a, s'')

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure
  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (>>=) (StateT g) f = StateT $ \s -> g s >>= \(a, s') -> (runStateT $ f a) s'

get :: State s s
get = state $ \s -> (s, s)

put :: s -> State s ()
put s = state $ \_ -> ((), s)

state :: (s -> (a, s)) -> State s a
state f = StateT $ Identity . f

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)
