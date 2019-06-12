module Typeclasses.Monoid where

import Prelude (Bool(..), undefined, Bounded(..))

import Unsorted (id)

import Typeclasses.Numerics
import Typeclasses.Ord
import Typeclasses.Semigroup

{-
The class of monoids (types with an associative binary operation that
has an identity). Instances should satisfy the following laws:

    x <> mempty = x
    mempty <> x = x
    x <> (y <> z) = (x <> y) <> z (Semigroup law)
    mconcat = foldr '(<>)' mempty

The method names refer to the monoid of lists under concatenation, but
there are many other instances.

Some types can be viewed as a monoid in more than one way, e.g. both
addition and multiplication on numbers. In such cases we often define
newtypes and make those instances of Monoid, e.g. Sum and Product.
-}

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
    where foldr f z =
            let go [] = z
                go (y:ys) = y `f` go ys 
            in go

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty :: Min a
  mempty = Min maxBound

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty :: Max a
  mempty = Max minBound

instance Monoid Any where
  mempty :: Any
  mempty = Any False

instance Monoid All where
  mempty :: All
  mempty = All True

instance Num a => Monoid (Sum a) where
  mempty :: Sum a
  mempty  = Sum 0

instance Num a => Monoid (Product a) where
  mempty :: Product a
  mempty  = Product 1

instance Monoid (Endo a) where
  mempty = Endo id

  
-------------------
--- COMBINATORS ---
-------------------

stimesMonoid :: (Integral b, Monoid a) => b -> a -> a 
stimesMonoid = undefined
