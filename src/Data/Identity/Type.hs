module Data.Identity.Type where

import Prelude (Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a }

newtype Identity a = Identity { runIdentity :: a }
  deriving Show
