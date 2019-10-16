module Typeclasses.Comonad.Extra where

import Typeclasses.Comonad.Class

import Data.Function


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
