module Typeclasses.Foldable.Extra where

import Prelude (Bool(..), Num(..), errorWithoutStackTrace, undefined)

import Typeclasses.Eq
import Typeclasses.Ord
import Typeclasses.Semigroup
import Typeclasses.Monoid
import Typeclasses.Monad
import Typeclasses.Foldable.Class
import Typeclasses.MonadPlus

import Data.Bool
import Data.Ordering
import Data.Maybe
import Data.Function


-----------------------
--- Monadic Actions ---
-----------------------

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = foldr ((>>) . f) (return ())

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ = flip mapM_

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (>>) (return ())

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f z0 ta = foldr f' return ta z0
  where
    f' x k z = f z x >>= k

foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ f b ta = foldM f b ta >> return ()


-------------------------
--- Specialized Folds ---
-------------------------

concat :: Foldable t => t [a] -> [a]
concat = foldr smoosh []
  where
    smoosh :: [a] -> [a] -> [a]
    smoosh xs [] = xs
    smoosh [] ys = ys
    smoosh (x:xs) ys = x : smoosh xs ys

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f = foldr (\a b -> smoosh (f a) b) []
  where
    smoosh :: [a] -> [a] -> [a]
    smoosh xs [] = xs
    smoosh [] ys = ys
    smoosh (x:xs) ys = x : smoosh xs ys

and :: Foldable t => t Bool -> Bool
and = getAll . foldMap All

or :: Foldable t => t Bool -> Bool
or = getAny . foldMap Any

any :: Foldable t => (a -> Bool) -> t a -> Bool
any f = getAny . foldMap (Any . f)

all :: Foldable t => (a -> Bool) -> t a -> Bool
all f = getAll . foldMap (All . f)

maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy = undefined

minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy = undefined


----------------
--- Searches ---
----------------

infix 4 `notElem`
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem = not ... elem

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find f ta = foldr (\a b -> if f a then Just a else b) Nothing ta

-- Definition from base:
--find p = getFirst . foldMap (\ x -> First (if p x then Just x else Nothing))
-- this wont work without a Monoid instance for First
