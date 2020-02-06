module Data.Tuple.Classes where

import Typeclasses.Monoid
import Typeclasses.Functor
import Typeclasses.Applicative.Class
import Typeclasses.Monad
import Typeclasses.Comonad


instance Functor ((,) a) where
  fmap :: (b -> c) -> (a, b) -> (a, c)
  fmap f (a, b) = (a, f b)

instance Monoid a => Applicative ((,) a) where
  pure :: b -> (a, b)
  pure a = (mempty, a)
  (<*>) :: (a, b -> c) -> (a, b) -> (a, c)
  (<*>) (a, f) (a1, a2) = (a `mappend` a1, f a2)

instance Monoid a => Monad ((,) a) where
  (>>=) :: (a, b) -> (b -> (a, c)) -> (a, c)
  (>>=) (a, b) f = let (a1, c) = f b in (a `mappend` a1, c)

instance Comonad ((,) a) where
  extract :: (a, b) -> b
  extract (a, b) = b
  extend :: ((a, c) -> b) -> (a, c) -> (a, b)
  extend f t@(a, b) = (a, f t)
  duplicate :: (a, b) -> (a, (a, b))
  duplicate (a, b) = (a, (a, b))

fst :: (a, b) -> a
fst (a, b) = a

snd :: (a, b) -> b
snd (a, b) = b

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
