> module Typeclasses.Arrow where

> import Prelude (undefined)

> import Typeclasses.Category
> import Data.Function

> class Category a => (Arrow a) where
>   arr :: (b -> c) -> a b c
>   first :: a b c -> a (b, d) (c, d)
>   second :: a b c -> a (d, b) (d, c)
>   (***) :: a b c -> a b' c' -> a (b, b') (c, c')
>   (&&&) :: a b c -> a b c' -> a b (c, c')
>   {-# MINIMAL arr, (first | (***)) #-}

