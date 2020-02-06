module Data.Stream.Type where

import Prelude (Show)

data Stream a = Stream a (Stream a)
  deriving Show
