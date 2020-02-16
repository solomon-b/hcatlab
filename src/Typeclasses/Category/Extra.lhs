> module Typeclasses.Category.Extra where

> import Data.Function (flip)

> import Typeclasses.Category.Class

Right-to-left composition

> infixr 1 <<<
> (<<<) :: Category cat => cat b c -> cat a b -> cat a c
> (<<<) = (.)

Left-to-right composition

> infixr 1 >>>
> (>>>) :: Category cat => cat a b -> cat b c -> cat a c
> (>>>) = flip (.)
