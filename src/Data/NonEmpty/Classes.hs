module Data.NonEmpty.Classes where

import Data.Function
import Data.List
import Data.NonEmpty.Type

import Typeclasses.Semigroup
import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad
import Typeclasses.Foldable
import Typeclasses.Traversable
import Typeclasses.Comonad


instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| xs ++ Cons y ys

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| Nil
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| (Cons f' fs)) xs = (f <$> xs) <> ((f' :| fs) <*> xs)
  (<*>) (f :| Nil) xs = f <$> xs

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| Nil) f = f x
  (>>=) (x :| (Cons x' xs)) f = f x <> ((x' :| xs) >>= f)

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
    where f Nil = Nil
          f (Cons y ys) = (y :| ys) `Cons` f ys
