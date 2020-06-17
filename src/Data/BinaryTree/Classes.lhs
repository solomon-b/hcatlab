> module Data.BinaryTree.Classes where

> import Data.BinaryTree.Type
> import Data.Either
> import Data.Function (($))
> import Typeclasses.Eq
> import Typeclasses.Functor
> import Typeclasses.Generic

> instance Generic (BinaryTree a) where
>   type Rep (BinaryTree a) =
>     Either () (Wrap (BinaryTree a), (Wrap a, Wrap (BinaryTree a)))
>
>   from :: BinaryTree a -> Rep (BinaryTree a)
>   from Leaf = Left ()
>   from (Node x a y) = Right (Wrap x, (Wrap a, Wrap y))
>
>   to :: Rep (BinaryTree a) -> BinaryTree a
>   to (Left ()) = Leaf
>   to (Right (Wrap x, ( Wrap a, Wrap y))) = Node x a y

> instance (Generic a, Eq a) => Eq (BinaryTree a) where
>   (==) = eq

> instance Functor BinaryTree where
>   fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
>   fmap f Leaf = Leaf
>   fmap f (Node x a y) = Node (fmap f x) (f a) (fmap f y)

> instance Functor Leafy where
>   fmap :: (a -> b) -> Leafy a -> Leafy b
>   fmap f (Tip a) = Tip (f a)
>   fmap f (Bin l r) = Bin (fmap f l) (fmap f r)


