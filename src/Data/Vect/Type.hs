module Data.Vect.Type where

import Data.Kind

data Nat = Z | S Nat
type Matrix n m a = Vect n (Vect m a)

infixr 5 :.
data Vect (n :: Nat) (a :: Type) where
  Nil :: Vect 'Z a
  (:.) :: a -> Vect n a -> Vect ('S n) a
