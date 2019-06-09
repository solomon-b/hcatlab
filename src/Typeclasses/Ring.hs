module Typeclasses.Ring where

import GHC.Integer (Integer)
import Typeclasses.Rig
import Typeclasses.Rng

class (Rng r, Rig r) => Ring r where
  fromInteger :: Integer -> r
