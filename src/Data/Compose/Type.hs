module Data.Compose.Type where

newtype Compose f g a = Compose (f (g a))
