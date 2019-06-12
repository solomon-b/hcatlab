module Data.Maybe where

import Prelude (Show)

import Data.Function (($))
import Typeclasses.Semigroup
import Typeclasses.Monoid
import Typeclasses.Functor

data Maybe a = Nothing | Just a
  deriving Show


-------------------
--- TYPECLASSES ---
-------------------
instance Semigroup a => Semigroup (Maybe a) where
  (<>) (Just a) (Just b) = Just $ a <> b
  (<>) ma Nothing = ma
  (<>) Nothing ma = ma

instance Semigroup a => Monoid (Maybe a) where
  mappend = (<>)
  mempty = Nothing

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x


-------------------
--- COMBINATORS ---
-------------------

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

