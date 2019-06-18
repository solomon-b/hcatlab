module Data.Stream where

import Prelude (Show)

import Data.Function (($))
import Typeclasses.Functor
import Typeclasses.Comonad

data Stream a = Stream a (Stream a)
  deriving Show


-------------------
--- TYPECLASSES ---
-------------------

instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (Stream a as) = Stream (f a) (fmap f as)

instance Comonad Stream where
  extract :: Stream a -> a
  extract (Stream x xs) = x
  extend :: (Stream a -> b) -> Stream a -> Stream b
  extend f s@(Stream x xs) = Stream (f s) (extend f xs)
  duplicate :: Stream a -> Stream (Stream a)
  duplicate s@(Stream x xs) = Stream s (duplicate xs)
