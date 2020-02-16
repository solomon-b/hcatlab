> module Data.Stream.Classes where

> import Data.Stream.Type

> import Typeclasses.Semigroup
> import Typeclasses.Foldable
> import Typeclasses.Functor
> import Typeclasses.Comonad

> instance Functor Stream where
>   fmap :: (a -> b) -> Stream a -> Stream b
>   fmap f (Stream a as) = Stream (f a) (fmap f as)

> instance Foldable Stream where
>   foldMap f (Stream a rest) = f a <> foldMap f rest

> instance Comonad Stream where
>   extract :: Stream a -> a
>   extract (Stream x xs) = x
>   extend :: (Stream a -> b) -> Stream a -> Stream b
>   extend f s@(Stream x xs) = Stream (f s) (extend f xs)
>   duplicate :: Stream a -> Stream (Stream a)
>   duplicate s@(Stream x xs) = Stream s (duplicate xs)
