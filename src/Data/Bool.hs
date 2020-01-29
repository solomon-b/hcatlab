module Data.Bool where

import Prelude (Bool(..))

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
(&&) True p  = p
(&&) False _ = False

infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) True _  = True
(||) False p = p

not :: Bool -> Bool
not True  = False
not False = True

otherwise :: Bool
otherwise = True

bool :: a -> a -> Bool -> a
bool x y False = x
bool x y True  = y

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  a _ = a
ifThenElse False _ b = b
