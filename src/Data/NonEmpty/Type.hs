module Data.NonEmpty.Type where

import Prelude (Show)

infixr 5 :|
data NonEmpty a = a :| [a] deriving Show
