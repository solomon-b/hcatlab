> module Typeclasses.Functor.Class where

= Functor

The Functor class is used for types that can be mapped over. Instances of Functor should satisfy the following laws:

== Laws

```
fmap id  ==  id
fmap (f . g)  ==  fmap f . fmap g
```

== Typeclass

> infixl 4 <$
> class Functor f where
>   fmap :: (a -> b) -> f a -> f b
>   (<$) :: a -> f b -> f a

