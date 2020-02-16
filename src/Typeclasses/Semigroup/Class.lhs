> module Typeclasses.Semigroup.Class where

> import Prelude (Show(..), errorWithoutStackTrace, undefined)

> import Typeclasses.Eq
> import Typeclasses.Ord
> import Typeclasses.Numerics

> import Data.Bool.Extra
> import Data.List.Type
> import Data.NonEmpty.Type

The class of semigroups (types with an associative binary operation).

Instances should satisfy the associativity law:
    x <> (y <> z) = (x <> y) <> z

> class Semigroup a where
>   infixr 6 <>
>   (<>) :: a -> a -> a
>   sconcat :: NonEmpty a -> a
>   sconcat (x :| xs) = go x xs
>     where
>       go b (Cons a as) = a <> go a as
>       go b Nil     = b
>   stimes :: Integral b => b -> a -> a
>   stimes y x
>     | y <= 0 = errorWithoutStackTrace "stimes: positive multiplier expected"
>     | otherwise = f x y
>       where
>         f x y
>           | even y = f (x <> x) (y `quot` 2)
>           | y == 1 = x
>           | otherwise = g (x <> x) (y `quot` 2) x
>         g x y z
>           | even y = g (x <> x) (y `quot` 2) z
>           | y == 1 = x <> z
>           | otherwise = g (x <> x) (y `quot` 2) (x <> z)
