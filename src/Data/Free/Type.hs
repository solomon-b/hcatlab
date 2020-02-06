module Data.Free.Type where

import Data.Kind -- From Base Currently

data Free (f :: Type -> Type) (a :: Type) = Pure a | Impure (f (Free f a))
