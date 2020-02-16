> module Data.Bool.Extra where

> import Data.Bool.Type

Logical AND Operator

> infixr 3 &&
> (&&) :: Bool -> Bool -> Bool
> (&&) True p  = p
> (&&) False _ = False

Logical OR Operator

> infixr 2 ||
> (||) :: Bool -> Bool -> Bool
> (||) True _  = True
> (||) False p = p

Logical NOT Operator

> not :: Bool -> Bool
> not True  = False
> not False = True

Useful helper for guards

> otherwise :: Bool
> otherwise = True

> bool :: a -> a -> Bool -> a
> bool x y False = x
> bool x y True  = y

> ifThenElse :: Bool -> a -> a -> a
> ifThenElse True  a _ = a
> ifThenElse False _ b = b
