module Data.Parser.Type where

import Data.String
import Data.Kind
import Data.Maybe

newtype Parser (a :: Type) = Parser { runParser :: String -> Maybe (a, String) }
