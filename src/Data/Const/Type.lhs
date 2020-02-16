> module Data.Const.Type where

> import Prelude (Show)

> newtype Const a b = Const { getConst :: a }
>   deriving Show
