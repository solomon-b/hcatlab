module Data.BinaryTree where

import Prelude (Show)

import Unsorted (($))
import Typeclasses.Functor

data BinaryTree a = Leaf a | Node (BinaryTree a) (BinaryTree a)

  
-------------------
--- TYPECLASSES ---
-------------------

instance Functor BinaryTree where
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node x y) = Node (fmap f x) (fmap f y)
