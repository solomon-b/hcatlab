> module Data.Function where

Right to left function composition operator

> infixr 9 .
> (.) :: (b -> c) -> (a -> b) -> (a -> c)
> (.) f g x = f (g x)

The Blackbird Combinator

> infixr 9 ...
> (...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
> (...) = (.) . (.)

Variations on the Blackbird

> infixr 9 ....
> (....) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
> (....) = (.) . (.) . (.)

> infixr 9 .....
> (.....) :: (e -> f) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f)
> (.....) = (.) . (.) . (.) . (.)

> const :: a -> b -> a
> const a _ = a

> flip :: (a -> b -> c) -> b -> a -> c
> flip f b a = f a b

Function Application Combinator

> infixr 0 $
> ($) :: (a -> b) -> a -> b
> ($) f = f

Reverse Function Application Combinator

> infixr 1 &
> (&) :: a -> (a -> b) -> b
> (&) a f = f a

The Identity Function

> id :: a -> a
> id a = a

The Fixed Point Combinator

> fix :: (a -> a) -> a
> fix f = let x = f x in x

on b u x y runs the binary function b on the results of applying unary function
to two arguments x and y. From the opposite perspective, it transforms two
inputs and combines the outputs.

> on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
> on f g = \x y -> g x `f` g y

> curry :: ((a, b) -> c) -> a -> b -> c
> curry f a b = f (a, b)

> uncurry :: (a -> b -> c) -> ((a, b) -> c)
> uncurry f (a, b) = f a b
