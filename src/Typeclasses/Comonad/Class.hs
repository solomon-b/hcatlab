module Typeclasses.Comonad.Class where

import Typeclasses.Functor

import Data.Function

class Functor w => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  duplicate :: w a -> w (w a)
  duplicate = extend id
  {-# MINIMAL extract, (duplicate | extend) #-}
