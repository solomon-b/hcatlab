> module Typeclasses.Category.Class where


> class Category (cat :: k -> k -> *) where
>   {-# MINIMAL id, (.) #-}
>   id  :: forall (a :: k). cat a a
>   (.) :: forall (b :: k) (c :: k) (a :: k). cat b c -> cat a b -> cat a c
