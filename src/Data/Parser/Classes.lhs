> module Data.Parser.Classes where

> import Data.Function
> import Data.Maybe
> import Data.Parser.Type

> import Typeclasses.Functor
> import Typeclasses.Applicative
> import Typeclasses.Monad

> instance Functor Parser where
>   fmap :: (a -> b) -> Parser a -> Parser b
>   fmap f (Parser p) = Parser $ \str ->
>     case p str of
>       Nothing -> Nothing
>       Just (a, str') -> pure (f a, str')

> instance Applicative Parser where
>   pure :: a -> Parser a
>   pure a = Parser $ \str -> Just (a, str)
>   (<*>) :: Parser (a -> b) -> Parser a -> Parser b
>   (<*>) (Parser f) (Parser a) = Parser $ \str ->
>     f str >>= \(g, str') -> a str' >>= \(a', str'') -> pure (g a', str'')

> instance Monad Parser where
>   (>>=) :: Parser a -> (a -> Parser b) -> Parser b
>   (>>=) (Parser g) f = Parser $ \str -> g str >>= \(a, str') -> let (Parser h) = f a in h str'
