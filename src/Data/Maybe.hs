module Data.Maybe ( module Data.Maybe
                  , module Data.Maybe.Type
                  , module Data.Maybe.Classes
                  ) where

import Data.Maybe.Type
import Data.Maybe.Classes


fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just a) = a

