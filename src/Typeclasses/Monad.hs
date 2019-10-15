module Typeclasses.Monad where

import Prelude (Bool, undefined)

import Typeclasses.Functor
import Typeclasses.Applicative.Class

import Data.Function
--import Data.List

{-
The Monad class defines the basic operations over a monad, a concept
from a branch of mathematics known as category theory. From the
perspective of a Haskell programmer, however, it is best to think of a
monad as an abstract datatype of actions. Haskell's do expressions
provide a convenient syntax for writing monadic expressions.

Instances of Monad should satisfy the following laws:

    return a >>= k  =  k a
    m >>= return  =  m
    m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h

Furthermore, the Monad and Applicative operations should relate as follows:

    pure = return
    (<*>) = ap

The above laws imply:

    fmap f xs  =  xs >>= return . f
    (>>) = (*>)

and that pure and (<*>) satisfy the applicative functor laws.
-}

class Applicative m => Monad m where
  return :: a -> m a
  infixl 1 >>=
  (>>=) :: m a -> (a -> m b) ->  m b
  infixl 1 >>
  (>>) :: m a -> m b -> m b
  (>>) = (*>)


-------------------
--- COMBINATORS ---
-------------------

infixr 1 =<<
(=<<) :: Monad m =>  (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

infixr 1 >=>
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = f a >>= g

infixr 1 <=<
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c 
(<=<) = flip (>=>)

join :: Monad m => m (m a) -> m a
join = (=<<) id

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= \a -> return $ f a

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ma >>= \a -> mb >>= \b -> return $ f a b

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ma >>= \a -> mb >>= \b -> mc >>= \c -> return $ f a b c

liftM4 :: Monad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftM4 f ma mb mc md = ma >>= \a -> mb >>= \b -> mc >>= \c -> md >>= \d -> return $ f a b c d

liftM5 :: Monad m => (a -> b -> c -> d -> e -> f) -> m a -> m b -> m c -> m d -> m e -> m f
liftM5 f ma mb mc md me = ma >>= \a -> mb >>= \b -> mc >>= \c -> md >>= \d -> me >>= \e -> return $ f a b c d e

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = ma >>= \a -> mf >>= \f -> return $ f a
