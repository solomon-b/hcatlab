module Data.NonEmpty where

import Prelude (Show)

infixr 5 :|
data NonEmpty a = a :| [a] deriving Show
