module Data.State.Type where

import Data.Identity
import Data.Kind

--newtype State s a = State { runState :: s -> (a, s) }
newtype StateT s (m :: Type -> Type) a = StateT { unStateT :: s -> m (a, s) }

type State s = StateT s Identity
