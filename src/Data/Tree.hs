module Data.Tree where

type Forest a = [Tree a]

data Tree a = Node
  { rootLabel :: a
  , subForest :: Forest a
  }
