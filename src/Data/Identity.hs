module Data.Identity where

import Prelude (Show)

import Unsorted (($))
import Typeclasses.Functor

newtype Identity a = Identity { runIdentity :: a }
  deriving Show


-------------------
--- TYPECLASSES ---
-------------------

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a
