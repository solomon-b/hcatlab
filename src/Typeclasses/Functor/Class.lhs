> module Typeclasses.Functor.Class where

Functor

  The Functor class is used for types that can be mapped over. Instances of Functor should satisfy the following laws:

fmap id  ==  id
fmap (f . g)  ==  fmap f . fmap g


> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

