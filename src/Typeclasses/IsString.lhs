> module Typeclasses.IsString where

> import Data.String.Type

> class IsString a where
>   fromString :: String -> a
