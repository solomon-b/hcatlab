module Data.Store where

import Prelude (Show, undefined)
import Typeclasses.Monoid
import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Comonad

import Data.Function

  
data Store s a = Store (s -> a) s


---------------------------
--- TYPECLASS INSTANCES ---
---------------------------

instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store g s) = Store (f . g) s

instance Monoid s => Applicative (Store s) where
  pure :: a -> Store s a
  pure a = Store (const a) mempty
  (<*>) :: Store s (a -> b) -> Store s a -> Store s b
  (<*>) (Store f s) (Store g s') = Store (f s . g) s'

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store f s) = f s
  extend :: (Store s a -> b) -> Store s a -> Store s b
  extend f store@(Store g s) = Store (const (f store)) s
  duplicate = undefined


-------------------
--- COMBINATORS ---
-------------------

store :: (s -> a) -> s -> Store s a
store = Store

runStore :: Store s a -> (s -> a, s)
runStore (Store f s) = (f, s)

pos :: Store s a -> s
pos (Store _ s) = s

seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s) = Store f (g s)

peek :: s -> Store s a -> a
peek s (Store f s') = f s

peeks :: (s -> s) -> Store s a -> a
peeks f (Store g s) = g (f s)

experiment :: Functor f => (s -> f s) -> Store s a -> f a 
experiment f (Store g s) = g <$> f s
