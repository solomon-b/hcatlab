> module Data.List.Type where

> import Prelude (Show)

> data List a = Nil | Cons a (List a)
>   deriving Show
