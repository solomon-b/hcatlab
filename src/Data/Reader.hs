module Data.Reader where

import Prelude (Show, undefined)

import Data.Function (($), (.), const)
import Typeclasses.Semigroup
import Typeclasses.Monoid
import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad

newtype Reader r a = Reader { runReader :: r -> a }


-------------------
--- TYPECLASSES ---
-------------------

instance Semigroup a => Semigroup (Reader r a) where
  (<>) r1 r2 = Reader $ \r -> runReader r1 r <> runReader r2 r

instance Monoid a => Monoid (Reader r a) where
  mempty = undefined
  mappend = (<>)
  mconcat = undefined

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f reader = Reader $ \a -> f $ runReader reader a

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) = undefined
  (*>) :: Reader r a -> Reader r b -> Reader r b
  (*>) = undefined
  (<*) :: Reader r a -> Reader r b -> Reader r a
  (<*) = undefined

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure
  (>>=) :: (a -> Reader r b) -> Reader r a -> Reader r b
  (>>=) f (Reader ra) = Reader $ \r ->
    let a = ra r
        (Reader rb) = f a
    in rb r
