module Data.Reader ( module Data.Reader
                   , module Data.Reader.Type
                   , module Data.Reader.Classes
                   ) where


import Data.Function
import Data.Reader.Type
import Data.Reader.Classes

ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader $ \r -> ra (f r)
