module Typeclasses.Comonad where

import Prelude (undefined)
import Typeclasses.Functor
import Data.Function
-- extend extract w = id w
-- extract . duplicate = id
-- extract id = () ????

class Functor w => Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  duplicate :: w a -> w (w a)
  duplicate = extend id
  {-# MINIMAL extract, (duplicate | extend) #-}


-------------------
--- COMBINATORS ---
-------------------

liftW :: Comonad w => (a -> b) -> w a -> w b 
liftW f = extend (f . extract)


wfix :: Comonad w => w (w a -> a) -> a 
wfix wwa =
  let f = extract wwa
      wa = extend f wa
  in extract wa

cfix :: Comonad w => (w a -> a) -> w a 
cfix f = let wa = extend f wa in wa

--kfix :: ComonadApply w => w (w a -> a) -> w a  
--kfix wwa =
--  let
--  in undefined
