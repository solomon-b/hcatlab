module Typeclasses.Traversable.Extra where

import Typeclasses.Functor
import Typeclasses.Monad
import Typeclasses.Traversable.Class

import Data.Function

-------------------
--- COMBINATORS ---
-------------------

traverse_ :: (Traversable t, Monad m) => (a -> m b) -> t a -> m ()
traverse_ = void ... traverse

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM = flip traverse

forM_ :: (Traversable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = void ... forM
