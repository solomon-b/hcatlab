module Typeclasses.Functor.Extra where

import Typeclasses.Functor.Class

import Data.Function

infixl 4 <$
(<$) :: Functor f => a -> f b -> f a
(<$) = fmap . const

infixl 4 $>
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

void :: Functor f => f a -> f ()
void = (<$) ()
