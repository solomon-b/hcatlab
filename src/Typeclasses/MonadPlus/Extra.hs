module Typeclasses.MonadPlus.Extra where

import Prelude (Bool, undefined)

import Typeclasses.MonadPlus.Class


mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a 
mfilter = undefined
