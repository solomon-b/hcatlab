module Data.Maybe.Type where

import Prelude (Show)

data MaybeT f a = NothingT | JustT (f a)
  deriving Show

-- type Maybe a = MaybeT Identity a
data Maybe a = Nothing | Just a
  deriving Show
