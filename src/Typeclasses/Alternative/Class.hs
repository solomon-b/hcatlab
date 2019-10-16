module Typeclasses.Alternative.Class where

import Typeclasses.Applicative.Class

class Applicative f => Alternative f where
  empty :: f a
  infixl 3 <|>
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
