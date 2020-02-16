> module Data.Store ( module Data.Store
>                   , module Data.Store.Type
>                   , module Data.Store.Classes
>                   ) where

> import Data.Store.Type
> import Data.Store.Classes

> import Typeclasses.Functor


> store :: (s -> a) -> s -> Store s a
> store = Store

> runStore :: Store s a -> (s -> a, s)
> runStore (Store f s) = (f, s)

> pos :: Store s a -> s
> pos (Store _ s) = s

> seek :: s -> Store s a -> Store s a
> seek s (Store f _) = Store f s

> seeks :: (s -> s) -> Store s a -> Store s a
> seeks g (Store f s) = Store f (g s)

> peek :: s -> Store s a -> a
> peek s (Store f s') = f s

> peeks :: (s -> s) -> Store s a -> a
> peeks f (Store g s) = g (f s)

> experiment :: Functor f => (s -> f s) -> Store s a -> f a
> experiment f (Store g s) = g <$> f s
