module Data.IO where

import Data.Function ((.), ($), id)
import Data.Kind

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad

import Prelude (String)
import qualified Prelude as P (getLine, putStrLn, Applicative(..), Monad(..), Functor(..), IO(..))

type IO = P.IO

instance Functor IO where
  fmap = P.fmap

instance Applicative IO where
  pure = P.pure
  (<*>) = (P.<*>)

instance Monad IO where
  return = pure
  (>>=) = (P.>>=)

getLine :: IO String
getLine = P.getLine

putStrLn :: String -> IO ()
putStrLn = P.putStrLn
