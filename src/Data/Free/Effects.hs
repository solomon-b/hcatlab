{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Data.Free.Effects where

import Data.Function
import Data.Free
import Data.IO
import Data.Kind
import Data.Maybe
import Data.Reader
import Data.State

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad

import Prelude (Int, String, undefined)

data Union :: [Type -> Type] -> Type -> Type where
  Here  :: f a        -> Union (f':fs) a
  There :: Union fs a -> Union (f':fs) a

instance Functor (Union '[]) where
  fmap f union = case union of {}

--instance Functor f => Functor (Union (f:fs)) where
--  fmap f (Here fa) = Here (f <$> fa)
--  fmap f (There union) = There (f <$> union)

class Member e es where
  inj :: e a -> Union es a
  prj :: Union es a -> Maybe (e a)

interpret :: (e a -> Union es a) -> Free (Union (e:es)) a -> Free (Union es) a
interpret = undefined

runM :: Monad m => Free (Union '[m]) a -> m a
runM =  undefined

run :: Free (Union '[]) a -> a
run (Pure a) = a
--run (Impure union) = let x = fmap run union in _

testUnion :: Union (Reader [String] ': IO ': fs) ()
testUnion = There $ Here undefined

testUnionF :: Functor (Union (Reader [String] ': IO ': fs)) => Free
       (Union (Reader [String] : IO : fs)) ()
testUnionF = eta testUnion
