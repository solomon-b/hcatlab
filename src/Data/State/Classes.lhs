> module Data.State.Classes where

> import Data.Function
> import Data.State.Type
> import Data.State.Extra

> import Typeclasses.Functor
> import Typeclasses.Applicative
> import Typeclasses.Monad

> instance Functor m => Functor (StateT s m) where
>   fmap :: (a -> b) -> StateT s m a -> StateT s m b
>   fmap f m = StateT $ \s ->  (\(a',s') -> (f a', s')) <$> runStateT m s

> instance Monad m => Applicative (StateT s m) where
>   pure :: a -> StateT s m a
>   pure a  = StateT $ \s -> pure (a, s)
>   (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
>   (<*>) (StateT mf) (StateT ma) = StateT $ \s ->
>     mf s >>= \(f, s') -> ma s' >>= \(a, s'') -> pure (f a, s'')

> instance Monad m => Monad (StateT s m) where
>   (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
>   (>>=) (StateT g) f = StateT $ \s -> g s >>= \(a, s') -> (runStateT $ f a) s'
