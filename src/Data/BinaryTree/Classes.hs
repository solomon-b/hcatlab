module Data.BinaryTree.Classes where

import Data.BinaryTree.Type
import Data.Function (($))
import Typeclasses.Functor


instance Functor BinaryTree where
  fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node x y) = Node (fmap f x) (fmap f y)
