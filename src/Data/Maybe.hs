module Data.Maybe where

import Prelude (Show, undefined)

import Data.Function

import Typeclasses.Semigroup
import Typeclasses.Monoid
import Typeclasses.Functor
import Typeclasses.Applicative
--import Typeclasses.Monad
--import Typeclasses.Foldable
--import Typeclasses.Traversable

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

instance Applicative Maybe where
  pure :: a -> Maybe a
  pure = Just
  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just a) = Just $ f a

--instance Monad Maybe where
--  return :: a -> Maybe a
--  return = pure
--  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--  (>>=) Nothing _ = Nothing
--  (>>=) (Just a) f = f a

--instance Foldable Maybe where
--  foldr :: (a -> b -> b) -> b -> Maybe a -> b
--  foldr _ _ Nothing = b
--  foldr f b (Just a) = f a b

--instance Traversable Maybe where
--  sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
--  sequenceA Nothing = pure Nothing
--  sequenceA (Just fa) = fmap Just fa


-------------------
--- COMBINATORS ---
-------------------

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

