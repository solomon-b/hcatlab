module Control.Applicative where

import Control.Functor
{-
A functor with application, providing operations to embed pure
expressions (pure), and sequence computations and combine their
results (<*> and liftA2).

A minimal complete definition must include implementations of pure and
of either <*> or liftA2. If it defines both, then they must behave the
same as their default definitions:

(<*>) = liftA2 id

liftA2 f x y = f <$> x <*> y

Further, any definition must satisfy the following:

identity
    pure id <*> v = v

composition
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

homomorphism
    pure f <*> pure x = pure (f x)

interchange
    u <*> pure y = pure ($ y) <*> u
-}

class Functor f => Applicative f where 
  pure :: a -> f a 
  infixl 4 <*>
  (<*>) :: f (a -> b) -> f a -> f b
  infixl 4 *>
  (*>) :: f a -> f b -> f b
  infixl 4 <*
  (<*) :: f a -> f b -> f a
