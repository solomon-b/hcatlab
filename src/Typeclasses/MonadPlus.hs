module Typeclasses.MonadPlus where

import Prelude (Bool, undefined)

import Typeclasses.Alternative
import Typeclasses.Monad

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mzero = empty
  mplus :: m a -> m a -> m a
  mplus = (<|>)

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a 
mfilter = undefined
