> module Typeclasses.Alternative.Extra where

> import Typeclasses.Functor
> import Typeclasses.Applicative
> import Typeclasses.Alternative.Class

> import Data.Maybe
> import Data.Function
>
> optional :: Alternative f => f a -> f (Maybe a)
> optional v = Just <$> v <|> pure Nothing
