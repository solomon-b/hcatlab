{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
module Data.State where

import Prelude (undefined)

import Typeclasses.Functor
import Typeclasses.Applicative.Class
import Typeclasses.Monad

import Data.Function
import Data.Identity
import Data.Tuple

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
--newtype State s a = State { runState :: s -> (a, s) }

type State s a = StateT s Identity a

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

---------------------------
--- TYPECLASS INSTANCES ---
---------------------------

--instance Functor (StateT s m) where
--  --fmap :: (a -> b) -> State s a -> State s b
--  --fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')
--
--instance Monad (StateT s m) where
--  return = undefined
--  (>>=) (StateT g) f = StateT $ \s ->

    
--instance Applicative (State s) where
--  pure :: a -> State s a
--  pure a = State $ \s -> (a, s)
--  (<*>) :: State s (a -> b) -> State s a -> State s b
--  (<*>) (State f) (State a) = State $
--    \s ->
--      let (a', s') = a s
--          (f', s'') = f s'
--      in (f' a', s'')
--
--instance Monad (State s) where
--  return :: a -> State s a
--  return = pure
--(>>=) :: State s a -> (a -> State s b) -> State s b
--(>>=) (State sa) f = State $ 
--  \s ->
--    let (a, s') = sa s
--    in runState (f a) s'
          
