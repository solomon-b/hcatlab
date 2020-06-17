> module Typeclasses.Generic where

> import Prelude (undefined)
> import Data.Bool
> import Data.Either
> import Data.Function
> import Typeclasses.Eq

> newtype Wrap a = Wrap a

> class Generic a where
>   type Rep a :: *
>
>   from :: a -> Rep a
>   to   :: Rep a -> a

> instance Generic () where
>   type Rep () = ()
>
>   from :: () -> Rep ()
>   from = id
>   to :: Rep () -> ()
>   to = id

> instance Generic a => Generic (Wrap a) where
>   type Rep (Wrap a) = Wrap a
>
>   from :: Wrap a -> Rep (Wrap a)
>   from = id
>   to :: Rep (Wrap a) -> Wrap a
>   to = id

> instance (Generic a, Generic b) => Generic (Either a b) where
>   type Rep (Either a b) = Either a b
>
>   from :: Either a b -> Rep (Either a b)
>   from = id
>
>   to :: Rep (Either a b) -> Either a b
>   to = id

> instance (Generic a, Generic b) => Generic (a, b) where
>   type Rep (a, b) = (a, b)
>
>   from :: (a, b) -> Rep (a, b)
>   from = id
>
>   to :: Rep (a, b) -> (a, b)
>   to = id

> class GEq a where
>   geq :: a -> a -> Bool

> instance GEq () where
>   geq :: () -> () -> Bool
>   geq _ _ = True

> instance (Generic a, Eq a) => GEq (Wrap a) where
>   geq (Wrap a) (Wrap b) = a == b

> instance (GEq a, GEq b) => GEq (Either a b) where
>   geq :: Either a b -> Either a b -> Bool
>   geq (Left x) (Left y) = geq x y
>   geq (Right x) (Right y) = geq x y
>   geq _ _ = False

> instance (GEq a, GEq b) => GEq (a, b) where
>  geq :: (a, b) -> (a, b) -> Bool
>  geq (x1, y1) (x2, y2) = geq x1 x2 && geq y1 y2

> eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
> eq x y = geq (from x) (from y)
