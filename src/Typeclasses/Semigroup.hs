module Typeclasses.Semigroup where

import Prelude (Show, Bool, errorWithoutStackTrace, undefined)

import Unsorted (($), (.))

import Typeclasses.Numerics
import Typeclasses.Ord
import Data.NonEmpty
import Data.Bool

{-
The class of semigroups (types with an associative binary operation).

Instances should satisfy the associativity law:
    x <> (y <> z) = (x <> y) <> z
-}

class Semigroup a where
  infixr 6 <>
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a 
  sconcat (x :| xs) = go x xs
    where
      go b (a:as) = a <> go a as
      go b []     = b
  stimes :: Integral b => b -> a -> a 
  stimes y x
    | y <= 0 = errorWithoutStackTrace "stimes: positive multiplier expected"
    | otherwise = undefined

------------------------
--- NEWTYPE WRAPPERS ---
------------------------

newtype Min a = Min { getMin :: a } deriving Show
newtype Max a = Max { getMax :: a } deriving Show

-- | The Boolean Semigroup under conjunction
newtype All = All { getAll :: Bool } deriving Show

-- | The Boolean Semigroup under disjunction
newtype Any = Any { getAny :: Bool } deriving Show

-- | Semigroup under addition
newtype Sum a = Sum { getSum :: a } deriving Show

-- | Semigroup under multiplication
newtype Product a = Product { getProduct :: a } deriving Show

-- | The Semigroup of endomorphisms under composition
newtype Endo a = Endo { appEndo :: a -> a }

-- | The dual of a Monoid, obtained by swapping the arguments of mappend
newtype Dual a = Dual { getDual :: a }

instance Semigroup () where
  (<>) _ _ = ()

instance Ord a => Semigroup (Min a) where
  (<>) (Min a) (Min b) = Min (min a b)

instance Ord a => Semigroup (Max a) where
  (<>) (Max a) (Max b) = Max $ max a b

instance Semigroup Any where
  (<>) (Any p) (Any q) = Any $ p || q

instance Semigroup All where
  (<>) (All p) (All q) = All $ p && q

instance Num a => Semigroup (Sum a) where
  (<>) (Sum x) (Sum y) = Sum $ x + y

instance Num a => Semigroup (Product a) where
  (<>) (Product x) (Product y) = Product $ x * y

instance Semigroup (Endo a) where
  (<>) (Endo f) (Endo g) = Endo $ f . g

instance Semigroup (Dual a) where
  (<>) = undefined
