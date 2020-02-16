> module Data.Store.Classes where

> import Data.Store.Type

> import Typeclasses.Monoid
> import Typeclasses.Functor
> import Typeclasses.Applicative.Class
> import Typeclasses.Comonad

> import Data.Function

> import Prelude (undefined)

> instance Functor (Store s) where
>   fmap :: (a -> b) -> Store s a -> Store s b
>   fmap f (Store g s) = Store (f . g) s

> instance Monoid s => Applicative (Store s) where
>   pure :: a -> Store s a
>   pure a = Store (const a) mempty
>   (<*>) :: Store s (a -> b) -> Store s a -> Store s b
>   (<*>) (Store f s) (Store g s') = Store (f s . g) s'

> instance Comonad (Store s) where
>   extract :: Store s a -> a
>   extract (Store f s) = f s
>   extend :: (Store s a -> b) -> Store s a -> Store s b
>   extend f store@(Store g s) = Store (const (f store)) s
>   duplicate = undefined
