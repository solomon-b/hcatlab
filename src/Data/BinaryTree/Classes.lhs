> module Data.BinaryTree.Classes where

> import Data.BinaryTree.Type
> import Data.Function (($))
> import Typeclasses.Functor

> instance Functor BinaryTree where
>   fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
>   fmap f Leaf = Leaf
>   fmap f (Node x a y) = Node (fmap f x) (f a) (fmap f y)

> instance Functor Leafy where
>   fmap :: (a -> b) -> Leafy a -> Leafy b
>   fmap f (Tip a) = Tip (f a)
>   fmap f (Bin l r) = Bin (fmap f l) (fmap f r)
