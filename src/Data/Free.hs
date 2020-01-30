module Data.Free where

import Data.Function ((.), ($), id, const)
import Data.IO
import Data.Kind

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad

import qualified System.Exit as E hiding (ExitSuccess)

data Free (f :: Type -> Type) (a :: Type) = Pure a | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure fa) = Impure (f <$$> fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) (Pure f) (Pure a) = Pure (f a)
  (<*>) (Pure f) (Impure fa) = Impure (f <$$> fa)
  (<*>) (Impure ff) fa = Impure $ (<*> fa) <$> ff

instance Functor f => Monad (Free f) where
  return = pure
  (>>=) (Pure a) f = f a
  (>>=) (Impure fa) f = Impure $ (>>= f) <$> fa

eta :: Functor f => f a -> Free f a
eta = Impure . fmap Pure

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _     (Pure a) = pure a
foldFree morph (Impure fa) = morph fa >>= foldFree morph

{-
Creating a Monad from a Functor:

type FState s = Free (StateT s Identity)

getF :: FState s s
getF = eta get

putF :: s -> FState s ()
putF = eta . put

runFState :: FState s a -> s -> (a, s)
runFState (Pure x) s = (x, s)
runFState (Impure m) s = let (m', s') = runState m s in runFState m' s'

testState :: FState Int Int
testState = putF 10 >> getF

runFState' :: FState s a -> s -> (a, s)
runFState' state = runIdentity . runStateT (foldFree id state)

test_run = runFState' testState 0

-}

{-
Factoring out impurity from monadic actions:

data TeletypeF a = PutStrLn String a | GetLine (String -> a) | ExitSuccess

instance Functor TeletypeF where
  fmap f (PutStrLn str a) = PutStrLn str (f a)
  fmap f (GetLine g)      = GetLine (f . g)
  fmap f ExitSuccess      = ExitSuccess

type Teletype = Free TeletypeF

putStrLn' :: String -> Teletype ()
putStrLn' str = eta $ PutStrLn str ()

getLine' :: Teletype String
getLine' = eta $ GetLine id

exitSuccess' :: Teletype r
exitSuccess' = eta ExitSuccess

-- Interpretation in IO
runTeletype :: Teletype r -> IO r
runTeletype (Pure r) = pure r
runTeletype (Impure (PutStrLn str t)) = putStrLn str >> runTeletype t
runTeletype (Impure (GetLine f))      = getLine     >>= runTeletype . f
runTeletype (Impure ExitSuccess)      = E.exitSuccess

echo :: Teletype ()
echo = getLine' >>= \str ->
          putStrLn' str >>
          exitSuccess' >>
          putStrLn' "Finished"

test_run = runTeletype echo
-}


