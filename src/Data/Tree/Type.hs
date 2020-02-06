module Data.Tree.Type where

import Data.List

type Forest a = List (Tree a)

data Tree a = Node
  { rootLabel :: a
  , subForest :: Forest a
  }
