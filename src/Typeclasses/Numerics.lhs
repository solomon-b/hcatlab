> module Typeclasses.Numerics where

> import Prelude (Word, Int, Integer, Float, Double, Enum, Show, undefined)
> import qualified Prelude as P (Num(..), Real(..), Integral(..))

> import Typeclasses.Eq
> import Typeclasses.Ord

> import Data.Bool
> import Data.Function


= Semiring

The Semiring class represents a type R with two binary operations: addition and
multiplication.

= Laws

== (R, +) is a commutative monoid with identity element 0

- Associativity: ∀a b c ∈ R. (a + b) + c = a + (b + c)
- Identity: ∃0 ∈ R. ∀a ∈ R. 0 + a = a = a + 0
- Commutivity: ∀a b. a + b = b + a

== (R, ·) is a monoid with identity element 1

- Associativity: ∀a b c ∈ R. (a · b) · c = a · (b · c)
- Identity: ∃1 ∈ R. ∀a ∈ R. 1 · a = a = a · 1

== Multiplication left and right distributes over addition

- Left Distribution: ∀a b c ∈ R. a · (b + c) = (a · b) + (a · c)
- Right Distribution: ∀a b c ∈ R. (a + b) · c = (a · c) + (b · c)

== Multiplication by 0 annihilates R:

-- ∀a. 0 · a = a · 0 = 0

> class Semiring r where
>   add  :: r -> r -> r
>   mul  :: r -> r -> r
>   one  :: r
>   zero :: r

> infixl 6 +
> (+) :: Semiring r => r -> r -> r
> (+) = add

> infixl 7 *
> (*) :: Semiring r => r -> r -> r
> (*) = mul

= Ring

A Ring is an Abelian Group with a second binary operation that is associative,
is distributive over the abelian group operation, and has an identity element.

= Laws

A ring is a set R equipped with two binary operations + and · satisfying the
 following three sets of axioms, called the ring axioms.

== R is an Abelian Group under addition

- Associativity: ∀a b c ∈ R. (a + b) + c = a + (b + c)
- Commutivity: ∀a b ∈ R. a + b = b + a
- Identity: ∃0 ∈ R. ∀a. a + 0 = a
- Additive Inverse: ∀a ∈ R. ∃-a ∈ R. a + (−a) = 0

== R is a Monoid under multiplication:

- Associativity: ∀a b c ∈ R. (a · b) · c = a · (b · c)
- Identity: ∃1 ∈ R. ∀a. a + 1 = a

== Multiplication is distributive with respect to addition:

- Left Distributivity: ∀a b c ∈ R. a ⋅ (b + c) = (a · b) + (a · c)
- Right Distributivity: ∀a b c ∈ R. (b + c) · a = (b · a) + (c · a)

> class Semiring r => Ring r where
>   negate' :: r -> r

> sub :: Ring r => r -> r -> r
> sub x y = x + (negate' y)

> infixl 6 -
> (-) :: Ring r => r -> r -> r
> (-) = sub

-----------
--- NUM ---
-----------

> class P.Num a => Num a where
>   (+) :: a -> a -> a
>   (+) = (P.+)
>   (-) :: a -> a -> a
>   (-) = (P.-)
>   (*) :: a -> a -> a
>   (*) = (P.*)
>   negate :: a -> a
>   negate = P.negate
>   abs :: a -> a
>   abs = P.abs
>   signum :: a -> a
>   signum = P.signum
>   fromInteger :: Integer -> a
>   fromInteger = P.fromInteger

> instance Num Word where
> instance Num Int where
> instance Num Integer where
> instance Num Float where
> instance Num Double where


----------------------
--- Ratio/Rational ---
----------------------

Arbitrary-precision rational numbers, represented as a ratio of
two 'Integer' values.  A rational number may be constructed using
the '%' operator.

> data Ratio a = !a :% !a  deriving (Show)
> type Rational = Ratio Integer

> instance Eq a => Eq (Ratio a) where
>   (==) (a :% b) (a' :% b') = a == a' && b == b'

> class  (Num a, Ord a) => Real a  where
>   toRational :: a -> Rational

instance Real Int  where
    toRational x = toInteger x :% 1

> ratioPrec, ratioPrec1 :: Int
> ratioPrec  = 7  -- Precedence of ':%' constructor
> ratioPrec1 = ratioPrec + 1

> infinity, notANumber :: Rational
> infinity   = 1 :% 0
> notANumber = 0 :% 0

(%) :: (Integral a) => a -> a -> Ratio a
 x % y = reduce (x * signum y) (abs y)
   where
     reduce :: Integral a => a -> a -> Ratio a
     reduce n y = (x `quot` d) :% (y `quot` d)
     d = gcd x y

 gcd :: Integral a => a -> a -> a
 gcd x y = gcd' (abs x) (abs y)
     where
       gcd' :: Integral a => a -> a -> a
       gcd' a 0  =  a
       gcd' a b  =  undefined -- gcd' b (a `rem` b)


----------------
--- INTEGRAL ---
----------------

Integral numbers, supporting integer division.

The Haskell Report defines no laws for 'Integral'. However, 'Integral'
instances are customarily expected to define a Euclidean domain and have the
following properties for the 'div'/'mod' and 'quot'/'rem' pairs, given
suitable Euclidean functions @f@ and @g@:

* @x@ = @y * quot x y + rem x y@ with @rem x y@ = @fromInteger 0@ or
@g (rem x y)@ < @g y@
* @x@ = @y * div x y + mod x y@ with @mod x y@ = @fromInteger 0@ or
@f (mod x y)@ < @f y@

An example of a suitable Euclidean function, for `Integer`'s instance, is
'abs'.

> class (Real a, Enum a) => Integral a where
>   quot :: a -> a -> a
>   rem  :: a -> a -> a
>   div  :: a -> a -> a
>   mod  :: a -> a -> a
>   quotRem :: a -> a -> (a, a)
>   divMod  :: a -> a -> (a, a)
>   toInteger :: a -> Integer


-------------------
--- COMBINATORS ---
-------------------


> even :: Integral a => a -> Bool
> even n = n `rem` 2 == 0

> odd :: Integral a => a -> Bool
> odd = not . even
