module Typeclasses.Ord where


import Data.Bool.Type
import Data.Ordering.Type

import Typeclasses.Eq
{-
The Ord class is used for totally ordered datatypes.

Instances of Ord can be derived for any user-defined datatype whose constituent types are in Ord. The declared order of the constructors in the data declaration determines the ordering in derived Ord instances. The Ordering datatype allows a single comparison to determine the precise ordering of two objects.

The Haskell Report defines no laws for Ord. However, <= is customarily expected to implement a non-strict partial order and have the following properties:

Transitivity
    if x <= y && y <= z = True, then x <= z = True
Reflexivity
    x <= x = True
Antisymmetry
    if x <= y && y <= x = True, then x == y = True

Note that the following operator interactions are expected to hold:

    x >= y = y <= x
    x < y = x <= y && x /= y
    x > y = y < x
    x < y = compare x y == LT
    x > y = compare x y == GT
    x == y = compare x y == EQ
    min x y == if x <= y then x else y = True
    max x y == if x >= y then x else y = True

Minimal complete definition: compare | (<=)
-}

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  infix 4 <
  (<) :: a -> a -> Bool
  infix 4 <=
  (<=) :: a -> a -> Bool
  infix 4 >
  (>) :: a -> a -> Bool
  infix 4 >=
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
