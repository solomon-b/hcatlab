module Data.Stream ( module Data.Stream
                   , module Data.Stream.Type
                   , module Data.Stream.Classes
                   ) where

import Prelude (Num(..), Int, fromInteger, (==))

import Data.Stream.Type
import Data.Stream.Classes


mkStream :: (a -> a) -> a -> Stream a
mkStream f a = Stream a (mkStream f (f a))

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS i (Stream a as) = a : takeS (i - 1) as
