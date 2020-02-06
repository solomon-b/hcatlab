module Data.Tree.Classes where

import Prelude (undefined)

import Typeclasses.Functor
import Typeclasses.Applicative.Class

import Data.Function
import Data.List.Type
import Data.Tree.Type


instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node a Nil) = Node (f a) Nil
  fmap f (Node a forest) = Node (f a) $ (fmap . fmap) f forest

instance Applicative Tree where
  pure :: a -> Tree a
  pure = flip Node Nil
  (<*>) fs as = undefined
