{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module FSchemeParser.BasicParser where

import Control.Applicative ( Alternative(..) )
import Control.Monad ( MonadPlus(..) )
import Data.Bifunctor ( Bifunctor(first) )
import Data.Maybe (isNothing)

newtype Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

instance Functor Parser where
  fmap f p = P $ \str -> fmap (first f) (parse p str)

instance Applicative Parser where
  pure a = P $ \str -> Just (a, str)
  pf <*> p = P $ \str -> case parse pf str of
    Nothing -> Nothing
    Just (f, str') -> parse (fmap f p) str'

instance Alternative Parser where
  empty = P $ const Nothing
  p <|> q = P $ \str -> if isNothing (parse p str) then parse q str else parse p str

instance Monad Parser where
  -- parse p str :: Maybe (a, String)
  -- fmap (first f) (parse p str) :: Maybe (Parser b, String)
  -- fmap (uncurry parse . first f) (parse p str) :: Maybe (Maybe (b, String))
  -- join $ fmap (uncurry parse . first f) (parse p str) :: Maybe (b, String)
  --  p >>= f = P $ \str -> Control.Monad.join $ fmap (uncurry parse . first f) (parse p str)
  p >>= f = P $ \str -> parse p str >>= uncurry parse . first f

class CMonoid a where
  cempty :: a
  cappend :: a -> a -> a
  (<?>) :: a -> a -> a
  (<?>) = cappend
  cconcat :: [a] -> a
  cconcat [] = cempty
  cconcat (x : xs) = x <?> cconcat xs

instance CMonoid (Maybe a) where
  cempty = Nothing
  cappend (Just x) _ = Just x
  cappend Nothing y = y

instance MonadPlus Parser where
  mzero = P $ const Nothing
  mplus p q = P $ \str -> parse p str <?> parse q str

item :: Parser Char
item = P $ \case
  "" -> Nothing
  (c : cs) -> Just (c, cs)

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else P (const Nothing)

oneOf :: String -> Parser Char
oneOf options = sat (`elem` options)

runNTimes :: Int -> Parser a -> Parser [a]
runNTimes 0 _ = return []
runNTimes n p = do
  a <- p
  as <- runNTimes (n - 1) p
  return (a:as)

parseWord :: String -> Parser String
parseWord "" = return ""
parseWord (l:ls) = do
  c <- sat (== l)
  cs <- parseWord ls
  return (c:cs)
