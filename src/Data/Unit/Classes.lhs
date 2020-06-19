> module Data.Unit.Classes where

> import Data.Unit.Type


> import Typeclasses.Generic

> instance Generic Unit where
>   type Rep Unit = Unit
>
>   from :: Unit -> Rep Unit
>   from _ = Unit
>
>   to :: Rep Unit -> Unit
>   to _ = Unit
