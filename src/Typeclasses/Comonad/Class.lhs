> module Typeclasses.Comonad.Class where

> import Data.Function

> import Typeclasses.Functor


> class Functor w => Comonad w where
>   {-# MINIMAL extract, (duplicate | extend) #-}
>   extract :: w a -> a
>   extend :: (w a -> b) -> w a -> w b
>   extend f = fmap f . duplicate
>   duplicate :: w a -> w (w a)
>   duplicate = extend id
