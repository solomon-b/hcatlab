> module Data.Reader.Type where

> import Data.Kind

> newtype Reader (r :: Type) (a :: Type) = Reader { runReader :: r -> a }
