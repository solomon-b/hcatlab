module Data.NonEmpty.Type where

import Data.List.Type

import Prelude (Show)

infixr 5 :|
data NonEmpty a = a :| (List a) deriving Show
