module Typeclasses.Applicative where

import Data.Function

import Typeclasses.Functor
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
  (<*>) = liftA2 id
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c 
  liftA2 f x = (<*>) (fmap f x)
  infixl 4 *>
  (*>) :: f a -> f b -> f b
  (*>) = liftA2 (flip const)
  infixl 4 <*
  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const


-------------------
--- COMBINATORS ---
-------------------

infixl 4 <**>
(<**>) :: Applicative f => f a -> f (a -> b) -> f b 
(<**>) = flip (<*>)

liftA :: Applicative f => (a -> b) -> f a -> f b 
liftA f a = pure f <*> a

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d 
liftA3 f a b c = liftA2 f a b <*> c
