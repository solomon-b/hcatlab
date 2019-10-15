module Typeclasses.Alternative where

import Typeclasses.Functor
import Typeclasses.Applicative.Class

import Data.Maybe
import Data.Function 

class Applicative f => Alternative f where
  empty :: f a
  infixl 3 <|>
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]

optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing
