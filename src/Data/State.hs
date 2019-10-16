{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
module Data.State where

import Prelude (undefined)

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad

import Data.Function
import Data.Identity
import Data.Tuple

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
--newtype State s a = State { runState :: s -> (a, s) }

type State s a = StateT s Identity a

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ pure . f

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
