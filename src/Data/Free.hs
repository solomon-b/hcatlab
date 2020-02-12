module Data.Free ( module Data.Free
                 , module Data.Free.Type
                 , module Data.Free.Classes
                 ) where

import Data.Function ((.), ($), id, const)
import Data.Free.Type
import Data.Free.Classes

import Typeclasses.Functor
import Typeclasses.Applicative
import Typeclasses.Monad
--import Data.IO

--import Data.State
--import Data.Identity
--import Prelude (String, Int, Num(..))

--import qualified System.Exit as E hiding (ExitSuccess)


eta :: Functor f => f a -> Free f a
eta = Impure . fmap Pure

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _     (Pure a) = pure a
foldFree morph (Impure fa) = morph fa >>= foldFree morph


{-
--Creating a Monad from a Functor:

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

test_run = runFState testState 0
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
