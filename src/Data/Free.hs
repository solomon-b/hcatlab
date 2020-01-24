module Data.Free where

import Data.Kind
import Data.Function ((.), ($))

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad

data Free (f :: Type -> Type) (a :: Type) = Pure a | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure fa) = Impure (f <$$> fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) (Pure f) (Pure a) = Pure (f a)
  (<*>) (Pure f) (Impure fa) = Impure (f <$$> fa)
  (<*>) (Impure ff) fa = Impure $ (<*> fa) <$> ff

instance Functor f => Monad (Free f) where
  return = pure
  (>>=) (Pure a) f = f a
  (>>=) (Impure fa) f = Impure $ (>>= f) <$> fa
