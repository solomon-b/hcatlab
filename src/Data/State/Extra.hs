module Data.State.Extra where

import Data.State.Type

import Data.Function
import Data.Identity
import Data.Tuple

import Typeclasses.Applicative
import Typeclasses.Monad

stateT :: Monad m => (s -> (a, s)) -> StateT s m a
stateT f = StateT $ pure . f

runStateT :: StateT s m a -> s -> m (a, s)
runStateT (StateT state) = state

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

get :: State s s
get = state $ \s -> (s, s)

put :: s -> State s ()
put s = state $ const ((), s)

state :: (s -> (a, s)) -> State s a
state f = StateT $ Identity . f

modify :: (s -> s) -> State s ()
modify f = state $ \s -> ((), f s)
