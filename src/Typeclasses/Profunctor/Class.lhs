> module Typeclasses.Profunctor.Class where

> import Data.Function
> import Data.Kind

> class Profunctor (p :: Type -> Type -> Type) where
>   dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
>   lmap  :: (a -> b) -> p b c -> p a c
>   lmap f = dimap f id
>   rmap  :: (b -> c) -> p a b -> p a c
>   rmap = dimap id

