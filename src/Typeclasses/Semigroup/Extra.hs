{-# LANGUAGE DerivingVia #-}
module Typeclasses.Semigroup.Extra where

import Prelude (Show(..)) --, errorWithoutStackTrace, undefined)

import Data.Bool.Type
import Data.Bool.Extra
import Data.Function (($), (.))
import Data.Maybe.Type

import Typeclasses.Numerics
import Typeclasses.Ord
import Typeclasses.Eq
import Typeclasses.Semigroup.Class
import Typeclasses.Functor.Class
import Typeclasses.Applicative.Class
import Typeclasses.Alternative.Class
import Typeclasses.Monad.Class
--import Typeclasses.Foldable.Class
--import Typeclasses.Traversable.Class


------------------------
--- NEWTYPE WRAPPERS ---
------------------------

newtype Id a = Id a

instance Functor Id where
  fmap f (Id a) = Id (f a)

newtype Min a = Min { getMin :: a }
  deriving Functor via Id
  deriving (Show, Eq)
  --deriving (Show, Eq, Ord, Functor, Applicative, Monad)

newtype Max a = Max { getMax :: a }
  deriving (Show, Eq, Ord, Functor, Applicative, Monad)

-- | The Boolean Semigroup under conjunction
newtype All = All { getAll :: Bool }
  deriving (Show, Eq, Ord)

-- | The Boolean Semigroup under disjunction
newtype Any = Any { getAny :: Bool }
  deriving (Show, Eq, Ord)

-- | Semigroup under addition
newtype Sum a = Sum { getSum :: a }
  deriving (Show, Eq, Ord, Functor, Applicative, Monad)

-- | Semigroup under multiplication
newtype Product a = Product { getProduct :: a }
  deriving (Show, Eq, Ord, Functor, Applicative, Monad)

-- | The Semigroup of endomorphisms under composition
newtype Endo a = Endo { appEndo :: a -> a }

-- | The dual of a Monoid, obtained by swapping the arguments of mappend
newtype Dual a = Dual { getDual :: a }
  deriving (Show, Eq, Ord, Functor, Applicative, Monad)

newtype First a = First { getFirst :: Maybe a }
  deriving (Show, Eq, Ord, Functor, Applicative, Monad)

newtype Last a = Last { getLast :: Maybe a }
  deriving (Show, Eq, Ord, Functor, Applicative, Monad)

newtype Ap f a = Ap { getApp :: f a }
  deriving (Show, Eq, Ord, Functor, Applicative, Alternative, Monad)

newtype Alt f a = Alt { getAlt :: f a }
  deriving (Show, Eq, Ord, Functor, Applicative, Alternative, Monad)

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

instance Semigroup a => Semigroup (Dual a) where
  (<>) (Dual x) (Dual y) = Dual $ y <> x
  stimes n (Dual x) = Dual $ stimes n x

instance Semigroup a => Semigroup (First a) where
  (<>) (First Nothing) b = b
  (<>) a               _ = a

instance Semigroup a => Semigroup (Last a) where
  (<>) a (Last Nothing) = a
  (<>) _ b               = b

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
  (<>) (Ap x) (Ap y) = Ap $ liftA2 (<>) x y

instance Alternative f => Semigroup (Alt f a) where
  (<>) (Alt x) (Alt y) = Alt $ x <|> y
