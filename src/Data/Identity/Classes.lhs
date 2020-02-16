> module Data.Identity.Classes where

> import Data.Function
> import Data.Identity.Type

> import Typeclasses.Semigroup
> import Typeclasses.Monoid
> import Typeclasses.Functor
> import Typeclasses.Applicative.Class
> import Typeclasses.Monad
> import Typeclasses.Foldable
> import Typeclasses.Traversable


> instance Semigroup a => Semigroup (Identity a) where
>   (<>) :: Identity a -> Identity a -> Identity a
>   (<>) (Identity a) (Identity b) = Identity $ a <> b

> instance Monoid a => Monoid (Identity a) where
>   mempty :: Identity a
>   mempty = Identity mempty
>   mappend :: Identity a -> Identity a -> Identity a
>   mappend = (<>)

> instance Functor Identity where
>   fmap :: (a -> b) -> Identity a -> Identity b
>   fmap f (Identity a) = Identity $ f a

> instance Applicative Identity where
>   pure :: a -> Identity a
>   pure = Identity
>   (<*>) :: Identity (a -> b) -> Identity a -> Identity b
>   (<*>) (Identity f) (Identity a) = Identity $ f a

> instance Monad Identity where
>   (>>=) :: Identity a -> (a -> Identity b) -> Identity b
>   (>>=) a f = f $ runIdentity a

> instance Foldable Identity where
>   foldr :: (a -> b -> b) -> b -> Identity a -> b
>   foldr f z (Identity a) = f a z

> instance Traversable Identity where
>   sequenceA :: Applicative f => Identity (f a) -> f (Identity a)
>   sequenceA (Identity fa) = fmap Identity fa
