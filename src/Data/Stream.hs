module Data.Stream where

import Prelude (Show)

import Data.Function (($))
import Typeclasses.Functor

data Stream a = Stream a (Stream a)
  deriving Show


-------------------
--- TYPECLASSES ---
-------------------

instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (Stream a as) = Stream (f a) (fmap f as)
