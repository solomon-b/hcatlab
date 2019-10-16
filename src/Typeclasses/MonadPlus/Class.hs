module Typeclasses.MonadPlus.Class where


import Typeclasses.Alternative.Class
import Typeclasses.Monad.Class

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mzero = empty
  mplus :: m a -> m a -> m a
  mplus = (<|>)
