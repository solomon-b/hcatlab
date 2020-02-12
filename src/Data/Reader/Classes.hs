module Data.Reader.Classes where

import Data.Function

import Data.Reader.Type

import Typeclasses.Semigroup
import Typeclasses.Monoid
import Typeclasses.Functor
import Typeclasses.Applicative.Class
import Typeclasses.Monad
import Typeclasses.Comonad
import Typeclasses.Contravariant

import Prelude (undefined)


instance Semigroup a => Semigroup (Reader r a) where
  (<>) r1 r2 = Reader $ \r -> runReader r1 r <> runReader r2 r

instance Monoid a => Monoid (Reader r a) where
  mempty :: Reader r a
  mempty = Reader $ const mempty
  mappend :: Reader r a -> Reader r a -> Reader r a
  mappend = (<>)
  mconcat :: [Reader r a] -> Reader r a
  mconcat = undefined

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f reader = Reader $ \a -> f $ runReader reader a

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) rf ra = Reader $ \r -> (runReader rf r) (runReader ra r)

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) (Reader ra) f = Reader $ \r ->
    let a = ra r
        (Reader rb) = f a
    in rb r
