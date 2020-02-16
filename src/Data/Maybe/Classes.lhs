> module Data.Maybe.Classes where

> import Data.Function

> import Data.Maybe.Type

> import Typeclasses.Semigroup.Class
> import Typeclasses.Monoid.Class
> import Typeclasses.Functor
> import Typeclasses.Applicative
> import Typeclasses.Monad

import Typeclasses.Foldable
import Typeclasses.Traversable


> instance Semigroup a => Semigroup (Maybe a) where
>   (<>) (Just a) (Just b) = Just $ a <> b
>   (<>) ma Nothing = ma
>   (<>) Nothing ma = ma

> instance Semigroup a => Monoid (Maybe a) where
>   mappend = (<>)
>   mempty = Nothing

> instance Functor Maybe where
>   fmap :: (a -> b) -> Maybe a -> Maybe b
>   fmap _ Nothing = Nothing
>   fmap f (Just x) = Just $ f x

> instance Applicative Maybe where
>   pure :: a -> Maybe a
>   pure = Just
>   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
>   (<*>) Nothing _ = Nothing
>   (<*>) (Just f) ma = f <$> ma

> instance Monad Maybe where
>   (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
>   (>>=) ma = join . flip fmap ma -- This might bottom due to join definition

instance Foldable Maybe where
  foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ _ Nothing = b
  foldr f b (Just a) = f a b

instance Traversable Maybe where
  traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse f Nothing = pure Nothing
  traverse f (Just x) = Just <$> f x
  sequenceA :: Applicative f => Maybe (f a) -> f (Maybe a)
  sequenceA Nothing = pure Nothing
  sequenceA (Just fa) = fmap Just fa
