module Typeclasses.Applicative.Extra where

import Prelude (Bool, undefined)

import Typeclasses.Functor
import Typeclasses.Applicative.Class
import Typeclasses.Foldable
  
import Data.Function


-------------------
--- COMBINATORS ---
-------------------

infixl 4 <**>
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

forever :: Applicative f => f a -> f b
forever a = let a' = a *> a' in a'

filterM :: Applicative f => (a -> f Bool) -> [a] -> m [a]
filterM f xs = undefined
--filterM f xs = undefined -- foldr (\x -> liftA2 $ undefined) (pure []) xs

mapAndUnzipM :: Applicative m => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM f xs =
  let mbcs = undefined -- f <$> xs
  in undefined

zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM = undefined

zipWithM_ :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ = undefined
