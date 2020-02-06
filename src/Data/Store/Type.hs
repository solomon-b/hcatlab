module Data.Store.Type where

import Data.Kind

data Store (s :: Type) (a :: Type) = Store (s -> a) s

