module Data.Tree where

import Prelude (Show)

import Typeclasses.Functor

import Data.Function (($), (.))
import Data.List


type Forest a = [Tree a]

data Tree a = Node
  { rootLabel :: a
  , subForest :: Forest a
  }


-------------------
--- TYPECLASSES ---
-------------------

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node a []) = Node (f a) []
  fmap f (Node a forest) = Node (f a) $ (fmap . fmap) f forest
