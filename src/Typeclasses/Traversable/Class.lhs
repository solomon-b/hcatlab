> module Typeclasses.Traversable.Class where

> import Data.Function

> import Typeclasses.Functor
> import Typeclasses.Applicative
> import Typeclasses.Monad
> import Typeclasses.Foldable.Class

Functors representing data structures that can be traversed from left to right.

A definition of traverse must satisfy the following laws:

naturality
    t . traverse f = traverse (t . f) for every applicative transformation t
identity
    traverse Identity = Identity
composition
    traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

A definition of sequenceA must satisfy the following laws:

naturality
    t . sequenceA = sequenceA . fmap t for every applicative transformation t
identity
    sequenceA . fmap Identity = Identity
composition
    sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA

> class (Functor t, Foldable t) => Traversable t where
>   {-# MINIMAL traverse | sequenceA #-}
>   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
>   traverse = sequenceA ... fmap
>   sequenceA :: Applicative f => t (f a) -> f (t a)
>   sequenceA = traverse id
>   mapM :: Monad m => (a -> m b) -> t a -> m (t b)
>   mapM = traverse
>   sequence :: Monad m => t (m a) -> m (t a)
>   sequence = sequenceA
