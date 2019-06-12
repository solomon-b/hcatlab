{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module Typeclasses.Eq where

import Prelude (Ordering, Bool)

import Data.Bool
{-
Reflexivity
    x == x = True
Symmetry
    x == y = y == x
Transitivity
    if x == y && y == z = True, then x == z = True
Substitutivity
    if x == y = True and f is a "public" function whose return type is an instance of Eq, then f x == f y = True
Negation
    x /= y = not (x == y)
-}

class Eq a where
  infix 4 ==
  (==) :: a -> a -> Bool
  (==) x y = not (x /= y)
  infix 4 /=
  (/=) :: a -> a -> Bool
  (/=) x y = not (x == y)

deriving instance Eq Bool
deriving instance Eq Ordering
