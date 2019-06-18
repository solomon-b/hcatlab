{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Typeclasses.Category where

class Category (cat :: k -> k -> *) where
  id :: forall (a :: k). cat a a
  (.) :: forall (b :: k) (c :: k) (a :: k). cat b c -> cat a b -> cat a c
  {-# MINIMAL id, (.) #-}
