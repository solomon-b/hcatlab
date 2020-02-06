module Data.Either.Classes where

import Data.Either.Type
import Data.Function

import Typeclasses.Semigroup
import Typeclasses.Functor
import Typeclasses.Applicative.Class
import Typeclasses.Monad
import Typeclasses.Foldable
import Typeclasses.Traversable

instance Semigroup (Either a b) where
  (<>) :: Either a b -> Either a b -> Either a b
  (<>) (Left a) (Left b) = Left b
  (<>) (Left a) _ = Left a
  (<>) _ (Right b) = Right b

instance Functor (Either c) where
  fmap :: (a -> b) -> Either c a -> Either c b
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right $ f x

instance Applicative (Either a) where
  pure :: b -> Either a b
  pure = Right
  (<*>) :: Either a (b -> c) -> Either a b -> Either a c
  (<*>) (Left err) _ = Left err
  (<*>) (Right f) b = f <$> b

instance Monad (Either a) where
  (>>=) :: Either a b -> (b -> Either a c) -> Either a c
  (>>=) e = join . flip fmap e 

instance Foldable (Either a) where
  foldr :: (b -> c -> c) -> c -> Either a b -> c
  foldr f z (Left _) = z
  foldr f z (Right b) = f b z

instance Traversable (Either a) where
  sequenceA :: Applicative f => Either a (f b) -> f (Either a b)
  sequenceA (Left err) = pure $ Left err
  sequenceA (Right ta) = fmap Right ta
