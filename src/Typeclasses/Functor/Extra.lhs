> module Typeclasses.Functor.Extra where

> import Typeclasses.Functor.Class
> import Data.Function


Replace all values in the Functor with a value.

> infixl 4 $>
> ($>) :: Functor f => f a -> b -> f b
> ($>) = flip (<$)

An infix synonym for fmap.

The $ implies the relation to function application:
```
(<$>) :: Functor f => (a -> b) -> f a -> f b
 ($)  ::              (a -> b) -> a    ->  b
```
Fmap is function application lifted into the Functor context.

> infixl 4 <$>
> (<$>) :: Functor f => (a -> b) -> f a -> f b
> (<$>) = fmap

Functors can be composed, thus fmap can be composed to lift functions into arbitrarily nested Functors:

> infixl 4 <$$>
> (<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
> (<$$>) = fmap . fmap

> infixl 4 <$$$>
> (<$$$>) :: (Functor f, Functor g, Functor g) => (a -> b) -> f (g (h a)) -> f (g (h b))
> (<$$$>) = fmap . fmap . fmap

> infixl 1 <&>
> (<&>) :: Functor f => f a -> (a -> b) -> f b
> (<&>) = flip fmap

> void :: Functor f => f a -> f ()
> void = (<$) ()
