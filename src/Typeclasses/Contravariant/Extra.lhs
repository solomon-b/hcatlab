> module Typeclasses.Contravariant.Extra where

> import Data.Bool
> import Data.Function

> import Typeclasses.Contravariant.Class

------------------------------
--- Contravariant Newtypes ---
------------------------------

> newtype Predicate a = Predicate { run :: a -> Bool }

> instance Contravariant Predicate where
>   contramap :: (a -> b) -> Predicate b -> Predicate a
>   contramap f (Predicate b) = Predicate $ \a -> b (f a)
