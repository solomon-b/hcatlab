{-# LANGUAGE EmptyCase #-}
module Data.Void where

import Typeclasses.Functor
data Void

absurd :: Void -> a
absurd a = case a of {}

vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd
