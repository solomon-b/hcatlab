module Typeclasses.Monoid.Class where

import Typeclasses.Semigroup.Class

{-
The class of monoids (types with an associative binary operation that
has an identity). Instances should satisfy the following laws:

    x <> mempty = x
    mempty <> x = x
    x <> (y <> z) = (x <> y) <> z (Semigroup law)
    mconcat = foldr '(<>)' mempty

The method names refer to the monoid of lists under concatenation, but
there are many other instances.

Some types can be viewed as a monoid in more than one way, e.g. both
addition and multiplication on numbers. In such cases we often define
newtypes and make those instances of Monoid, e.g. Sum and Product.
-}

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
    where foldr f z =
           let go [] = z
               go (y:ys) = y `f` go ys 
           in go
