module MonadPlus where

import Typeclasses.Alternative
import Typeclasses.Monad

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mzero = empty
  mplus :: m a -> m a -> m a
  mplus = (<|>)
