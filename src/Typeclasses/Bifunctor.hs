module Typeclasses.Bifunctor where

{-
A bifunctor is a type constructor that takes two type arguments and is
a functor in both arguments. That is, unlike with Functor, a type
constructor such as Either does not need to be partially applied for a
Bifunctor instance, and the methods in this class permit mapping
functions over the Left value or the Right value, or both at the same
time.

Formally, the class Bifunctor represents a bifunctor from Hask -> Hask.

Intuitively it is a bifunctor where both the first and second
arguments are covariant.

You can define a Bifunctor by either defining bimap or by defining
both first and second.

If you supply bimap, you should ensure that:

  bimap id id ≡ id

If you supply first and second, ensure:

  first id ≡ id
  second id ≡ id

If you supply both, you should also ensure:

  bimap f g ≡ first f . second g

These ensure by parametricity:

  bimap  (f . g) (h . i) ≡ bimap f h . bimap g i
  first  (f . g) ≡ first  f . first  g
  second (f . g) ≡ second f . second g
-}

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d 
  first :: (a -> b) -> p a c -> p b c 
  second :: (b -> c) -> p a b -> p a c 
