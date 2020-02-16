> module Typeclasses.Monad.Extra where

> import Data.Function

> import Typeclasses.Functor.Class
> import Typeclasses.Applicative.Class
> import Typeclasses.Monad.Class


-------------------
--- COMBINATORS ---
-------------------

> infixr 1 =<<
> (=<<) :: Monad m =>  (a -> m b) -> m a -> m b
> (=<<) = flip (>>=)

> infixr 1 >=>
> (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
> (>=>) f g a = f a >>= g

> infixr 1 <=<
> (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c 
> (<=<) = flip (>=>)

> join :: Monad m => m (m a) -> m a
> join = (=<<) id

> liftM :: Monad m => (a -> b) -> m a -> m b
> liftM f ma = ma >>= \a -> pure $ f a

> liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
> liftM2 f ma mb = ma >>= \a -> mb >>= \b -> pure $ f a b

> liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
> liftM3 f ma mb mc = ma >>= \a -> mb >>= \b -> mc >>= \c -> pure $ f a b c

> liftM4 :: Monad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
> liftM4 f ma mb mc md = ma >>= \a -> mb >>= \b -> mc >>= \c -> md >>= \d -> pure $ f a b c d

> liftM5 :: Monad m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
> liftM5 f ma mb mc md me = ma >>= \a -> mb >>= \b -> mc >>= \c -> md >>= \d -> me >>= \e -> pure $ f a b c d e

> ap :: Monad m => m (a -> b) -> m a -> m b
> ap mf ma = ma >>= \a -> mf >>= \f -> pure $ f a
