module Typeclasses.Functor where

import Data.List
import Unsorted
{-
The Functor class is used for types that can be mapped over. Instances of Functor should satisfy the following laws:

fmap id  ==  id
fmap (f . g)  ==  fmap f . fmap g
-}


class Functor f where
  fmap :: (a -> b) -> f a -> f b

infixl 4 <$
(<$) :: Functor f => a -> f b -> f a
(<$) = fmap . const

infixl 4 $>
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

void :: Functor f => f a -> f ()
void = (<$) ()

-----------------
--- INSTANCES ---
-----------------

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap f Nil = Nil
