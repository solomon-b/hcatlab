> module Typeclasses.Contravariant.Class where

> import Prelude (Int, fromInteger, mod, Eq(..))
> import Data.Maybe
> import Data.Bool
> import Data.Function

> class Contravariant f where
>   contramap :: (a -> b) -> f b -> f a
>   infixl 4 >$
>   (>$) :: b -> f b -> f a
>   (>$) = contramap . const

> infixl 4 $<
> ($<) :: Contravariant f => f b -> b -> f a
> ($<) = flip (>$)

> infixl 4 >$<
> (>$<) :: Contravariant f => (a -> b) -> f b -> f a
> (>$<) = contramap

