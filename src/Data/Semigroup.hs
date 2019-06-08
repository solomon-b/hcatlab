{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Semigroup where

{-
The class of semigroups (types with an associative binary operation).

Instances should satisfy the associativity law:
    x <> (y <> z) = (x <> y) <> z
-}

class Semigroup a where
  infixr 6 <>
  (<>) :: a -> a -> a
