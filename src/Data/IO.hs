module Data.IO ( module Data.IO
               , module Data.IO.Type
               , module Data.IO.Classes
               ) where

import Data.IO.Type
import Data.IO.Classes
import Data.String (String)

import qualified Prelude (getLine, putStrLn)


getLine :: IO String
getLine = Prelude.getLine

putStrLn :: String -> IO ()
putStrLn = Prelude.putStrLn
