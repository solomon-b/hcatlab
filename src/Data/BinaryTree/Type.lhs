> module Data.BinaryTree.Type where

> import Prelude (Show)

The traditional decorated node Binary Tree which does not permit a monad instance.

> data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
>   deriving Show

A leafy form of Binary Tree which permits a Monad instance.

> data Leafy a = Tip a | Bin (Leafy a) (Leafy a)
>   deriving Show
