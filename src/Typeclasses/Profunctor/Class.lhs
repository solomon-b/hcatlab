> module Typeclasses.Profunctor.Class where

> import Data.Function
> import Data.Kind

= Profunctor
Formally, the class Profunctor represents a profunctor from Hask -> Hask.

Intuitively it is a bifunctor where the first argument is contravariant and the
second argument is covariant.

You can define a Profunctor by either defining dimap or by defining both lmap
and rmap.

== Laws
dimap id id ≡ id
lmap id ≡ id
rmap id ≡ id
dimap f g ≡ lmap f . rmap g
dimap (f . g) (h . i) ≡ dimap g h . dimap f i
lmap (f . g) ≡ lmap g . lmap f
rmap (f . g) ≡ rmap f . rmap g


> class Profunctor (p :: Type -> Type -> Type) where
>   {-# MINIMAL dimap | (lmap, rmap) #-}
>   dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
>   dimap f g = lmap f . rmap g
>   lmap  :: (a -> b) -> p b c -> p a c
>   lmap f = dimap f id
>   rmap  :: (b -> c) -> p a b -> p a c
>   rmap = dimap id

