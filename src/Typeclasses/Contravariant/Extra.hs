module Typeclasses.Contravariant.Extra where

import Prelude (fromInteger, (>))

import Data.Bool
import Data.Compose
import Data.Function

import Typeclasses.Contravariant.Class
import Typeclasses.Functor.Class

------------------------------
--- Contravariant Newtypes ---
------------------------------

newtype Predicate a = Predicate { run :: a -> Bool }

instance Contravariant Predicate where
  contramap :: (a -> b) -> Predicate b -> Predicate a
  contramap f (Predicate b) = Predicate $ \a -> b (f a)

---------------------
--- ContraCompose ---
---------------------
-- TODO: Move to its own module

newtype ContraCompose f g a = ContraCompose (f (g a))

instance (Contravariant f, Contravariant g) => Functor (ContraCompose f g) where
  fmap f (ContraCompose fg) = ContraCompose $ (contramap . contramap) f fg

