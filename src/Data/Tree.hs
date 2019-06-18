module Data.Tree where

import Prelude (Show, undefined)

import Typeclasses.Functor
import Typeclasses.Applicative

import Data.Function
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

instance Applicative Tree where
  pure :: a -> Tree a
  pure = flip Node []
  (<*>) fs as = undefined
