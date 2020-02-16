> module Data.Bool.Classes where

> import Data.Bool.Type

> import Typeclasses.Eq
> import Typeclasses.Ord

> instance Eq Bool where
>    (==) True  True  = True
>    (==) False False = True
>    (==) _     _     = False

Additional Instances TBD:
Bounded Bool

Since: base-2.1
Enum Bool

Since: base-4.0.0.0
Ord Bool
Read Bool

Since: base-2.1
Show Bool

Since: base-2.1
Ix Bool

Since: base-2.1
Generic Bool

FiniteBits Bool

Since: base-4.7.0.0
Bits Bool

Interpret Bool as 1-bit bit-field

Since: base-4.7.0.0
Storable Bool

Since: base-2.1
type Rep Bool
