module Data.BinaryTree.Type where

import Prelude (Show)


data BinaryTree a = Leaf a | Node (BinaryTree a) (BinaryTree a)
  deriving Show
