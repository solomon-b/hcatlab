module Typeclasses.Traversable where

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad
import Typeclasses.Foldable

import Data.Function

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
  traverse f = sequenceA . fmap f
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  mapM = traverse
  sequence :: Monad m => t (m a) -> m (t a)
  sequence = sequenceA


-------------------
--- COMBINATORS ---
-------------------

--mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b) 
--mapM = traverse
  
--mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m () 
--mapM_ f ta = 

--forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b) 
--forM = flip mapM
--
--forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m () 
--forM_ = flip mapM_
