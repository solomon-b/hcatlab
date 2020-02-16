> module Data.IO.Classes where

> import Data.IO.Type

> import Typeclasses.Functor
> import Typeclasses.Applicative
> import Typeclasses.Monad

> import qualified Prelude (Applicative(..), Monad(..), Functor(..))

> instance Functor IO where
>   fmap = Prelude.fmap

> instance Applicative IO where
>   pure = Prelude.pure
>   (<*>) = (Prelude.<*>)

> instance Monad IO where
>   (>>=) = (Prelude.>>=)
