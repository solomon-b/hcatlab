module Typeclasses.Monoid.Extra where

import Prelude (undefined, Bounded(..))

import Data.Bool.Type
import Data.Function (id)

import Typeclasses.Ord
import Typeclasses.Numerics
import Typeclasses.Semigroup.Extra
import Typeclasses.Monoid.Class

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

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty

--instance Monoid f => Monoid (App (f a)) where
--  mempty = App mempty

-------------------
--- COMBINATORS ---
-------------------

stimesMonoid :: (Integral b, Monoid a) => b -> a -> a 
stimesMonoid = undefined
