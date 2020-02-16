module Data.Compose.Classes where

import Data.Compose.Type
import Data.Function

import Typeclasses.Functor


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fg) = Compose $ (fmap . fmap) f fg
