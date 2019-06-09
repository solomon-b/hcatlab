module Data.Maybe where

import Prelude (Show)

import Unsorted (($))
import Typeclasses.Functor

data Maybe a = Nothing | Just a
  deriving Show


-------------------
--- TYPECLASSES ---
-------------------

instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just $ f x
