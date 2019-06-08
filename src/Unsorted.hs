{-# LANGUAGE NoImplicitPrelude #-}
module Unsorted where

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

