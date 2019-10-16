module Data.NonEmpty where

import Data.NonEmpty.Type
import Data.Function

import Typeclasses.Semigroup
import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad
import Typeclasses.Foldable
import Typeclasses.Traversable
import Typeclasses.Comonad

import Data.List

---------------------------
--- TYPECLASS INSTANCES ---
---------------------------

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ y:ys)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| []
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| (f':fs)) xs = (f <$> xs) <> ((f' :| fs) <*> xs)
  (<*>) (f :| []) xs = f <$> xs

instance Monad NonEmpty where
  return :: a -> NonEmpty a
  return = pure
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| []) f = f x
  (>>=) (x :| (x':xs)) f = f x <> ((x' :| xs) >>= f)

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

instance Traversable NonEmpty where
  sequenceA :: Applicative f => NonEmpty (f a) ->  f (NonEmpty a)
  sequenceA (f :| fs) = (:|) <$> f <*> (sequenceA fs)

instance Comonad NonEmpty where
  extract :: NonEmpty a -> a
  extract (x :| _) = x
  extend :: (NonEmpty a -> b) -> NonEmpty a -> NonEmpty b
  extend f (x :| xs) = f (x :| xs) :| (f . pure <$> xs)
  duplicate xs@(_ :| xs') = xs :| f xs'
    where f [] = []
          f (y:ys) = (y :| ys) : f ys

