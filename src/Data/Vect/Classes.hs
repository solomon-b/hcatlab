module Data.Vect.Classes where

import Data.Vect.Type

import Typeclasses.Semigroup.Class
import Typeclasses.Monoid.Class
import Typeclasses.Functor.Class
import Typeclasses.Foldable.Class

instance Show a => Show (Vect n a) where
  show Nil = "()"
  show (a :. Nil) = "(" <> show a <> ")"
  show vect = "(" <> f vect <> ")"
    where f :: Show a => Vect n a -> String
          f (a :. Nil) = show a
          f (a :. as) = show a <> ", " <> f as

instance Semigroup (Vect 'Z a) where
  (<>) :: Vect 'Z a -> Vect 'Z a -> Vect 'Z a
  (<>) Nil Nil = Nil

instance (Semigroup (Vect n a), Semigroup a) => Semigroup (Vect ('S n) a) where
  (<>) :: Vect ('S n) a -> Vect ('S n) a -> Vect ('S n) a
  (<>) (a :. as) (b :. bs) = (a <> b) :. (as <> bs)

instance Monoid (Vect 'Z a) where
  mempty = Nil

instance (Monoid (Vect n a), Monoid a) => Monoid (Vect ('S n) a) where
  mempty = mempty :. mempty

instance Functor (Vect n) where
  fmap :: (a -> b) -> Vect n a -> Vect n b
  fmap _ Nil = Nil
  fmap f (a :. as) = (f a) :. (f <$> as)

instance Foldable (Vect n) where
  foldMap :: Monoid m => (a -> m) -> Vect n a -> m
  foldMap _ Nil = mempty
  foldMap f (a :. as) = f a <> foldMap f as

