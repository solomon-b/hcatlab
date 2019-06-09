module Data.Either where

import Prelude (Show)

import Unsorted (($))
import Typeclasses.Functor

data Either a b = Left a | Right b
  deriving Show

-------------------
--- TYPECLASSES ---
-------------------

instance Functor (Either c) where
  fmap :: (a -> b) -> Either c a -> Either c b
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right $ f x
