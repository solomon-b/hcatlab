module Typeclasses.Foldable where

import Prelude (Bool(..), Num(..), errorWithoutStackTrace)

import Data.Coerce

import Unsorted (($), (.))

import Typeclasses.Eq
import Typeclasses.Ord
import Typeclasses.Ring
import Typeclasses.Semigroup
import Typeclasses.Monoid
import Typeclasses.Functor

import Data.Maybe


-- (#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
-- (#.) f g = \x -> f 
{-
Data structures that can be folded.

For example, given a data type

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

a suitable instance would be

instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

This is suitable even for abstract types, as the monoid is assumed to satisfy the monoid laws. Alternatively, one could define foldr:

instance Foldable Tree where
   foldr f z Empty = z
   foldr f z (Leaf x) = f x z
   foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

Foldable instances are expected to satisfy the following laws:

  foldr f z t = appEndo (foldMap (Endo . f) t ) z
  foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
  fold = foldMap id
  length = getSum . foldMap (Sum . const  1)

sum, product, maximum, and minimum should all be essentially
equivalent to foldMap forms, such as:

  sum = getSum . foldMap Sum

but may be less defined.

If the type is also a Functor instance, it should satisfy:

  foldMap f = fold . fmap f

which implies that:

  foldMap f . fmap g = foldMap (f . g)
-}

class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap f = foldr (mappend . f) mempty
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z t = appEndo (foldMap (Endo . f) t) z
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  elem :: Eq a => a -> t a -> Bool
  elem a = any (== a)
  maximum :: Ord a => t a -> a
  maximum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") . fmap getMax . foldMap (Just . Max)
  minimum :: Ord a => t a -> a
  minimum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") . fmap getMin . foldMap (Just . Min)
  sum :: Num a => t a -> a
  sum = foldr (+) 0
  product :: Num a => t a -> a
  product = foldr (*) 1


any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny . foldMap (Any . p)
