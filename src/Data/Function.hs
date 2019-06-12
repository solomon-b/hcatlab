module Data.Function where

infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

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
 
