module Data.Function where

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

infixr 9 ...
(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)

infixr 9 ....
(....) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(....) = (.) . (.) . (.)

const :: a -> b -> a
const a _ = a

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

infixr 0 $
($) :: (a -> b) -> a -> b
($) f = f

infixr 1 &
(&) :: a -> (a -> b) -> b
(&) a f = f a

id :: a -> a
id a = a

fix :: (a -> a) -> a
fix f = let x = f x in x
 
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b
