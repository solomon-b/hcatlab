{-# LANGUAGE LambdaCase #-}
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
  Here  :: f a        -> Union (f ': fs) a
  There :: Union fs a -> Union (f ': fs) a

instance Functor (Union '[]) where
  fmap f = \case{}

instance (Functor f, Functor (Union fs)) => Functor (Union (f ': fs)) where
  fmap f (Here fa) = Here (f <$> fa)
  fmap f (There union) = There (f <$> union)

class Member e es where
  inj :: e a -> Union es a
  prj :: Union es a -> Maybe (e a)

instance {-# OVERLAPPING #-} Member e (e ': es) where
  inj = Here
  prj (Here fa) = Just fa
  prj (There union) = Nothing

instance Member e es => Member e (f ': es) where
  inj = There . inj
  prj (Here ea) = Nothing
  prj (There union) = prj union

interpret :: Functor (Union es) => (forall a. e a -> Union es a) -> Free (Union (e:es)) a -> Free (Union es) a
interpret f (Pure a) = Pure a
interpret f (Impure (Here eff)) = Impure $ interpret f <$> f eff
interpret f (Impure (There union)) = Impure $ interpret f <$> union

runM :: Monad m => Free (Union '[m]) a -> m a
runM (Pure a) = pure a
runM (Impure (Here eff)) = eff >>= runM
runM (Impure (There union)) = let x = fmap runM union in _

run :: Free (Union '[]) a -> a
run (Pure a) = a
run (Impure union) = case union of {}


testUnion :: Union (Reader [String] ': IO ': fs) ()
testUnion = There $ Here undefined

testUnionF :: Functor (Union (Reader [String] ': IO ': fs)) => Free
       (Union (Reader [String] : IO : fs)) ()
testUnionF = eta testUnion
