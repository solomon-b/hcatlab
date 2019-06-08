{-# LANGUAGE NoImplicitPrelude #-}
module Data.Traversable where

import Control.Functor
import Control.Applicative
import Control.Monad
import Data.Foldable

{-
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
-}
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a) 
