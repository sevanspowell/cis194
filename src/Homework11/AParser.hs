{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Homework11.AParser (Parser, runParser, satisfy, char, posInt) where

import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- | Take a function which maps one parsing function to another, a Parser a, and
--   produce a Parser b. The Parser b runs Parser a, but also runs the given
--   function after running Parser a.
inParser :: ((String -> Maybe (a, String)) -> (String -> Maybe (b, String))) -> Parser a -> Parser b
inParser f = Parser . f . runParser

-- | Apply the given function to the first element of a pair only
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap = inParser . fmap . fmap . first
  -- (a -> b)
  -- first :: (a -> b) -> (a, c) -> (b, c)
  -- fmap  :: ((a, c) -> (b, c)) -> f (a, c) -> f (b, c)
  -- fmap' :: (f (a, c) -> f (b, c)) -> g f (a, c) -> g f (b, c)

  -- To fit 'inParser', f = Maybe, g  = ((->) String), c = String, making fmap':

  -- fmap' :: (Maybe (a, String) -> Maybe (b, String)) -> (String -> Maybe (a, String)) -> (String -> Maybe (a, String))
  -- fmap  :: ((a, String) -> (b, String)) -> Maybe (a, String) -> Maybe (b, String)
  -- first :: (a -> b) -> (a, String) -> (b, String)

  -- Which makes sense :)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\str -> Just (x, str))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1f) <*> p2 = Parser $ \s ->
    case p1f s of
      Nothing      -> Nothing
      Just (f, s') -> runParser (f <$> p2) s' -- f :: (Parser a -> Parser b)
  -- p1f :: String -> Maybe ((a -> b), String)
  -- p1f :: Maybe ((a -> b), String)
  -- f <$> p2 :: Parser b


(*>) :: Applicative f => f a -> f b -> f b
-- (*>) a b = flip const <$> a <*> b
(*>) = liftA2 (flip const)

-- mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
-- mapA f = (\xs -> (pure (map f)) <*> (pure xs))
-- Lift xs into context of Applicative Functor
-- Lift 'map f' into context of Applicative Functor
-- Apply Applicative f to Applicative list

-- (<*>) :: f ([a] -> [b]) -> f ([a]) -> f ([b])

mySequenceA :: Applicative f => [f a] -> f [a]
mySequenceA [] = pure []
mySequenceA (x:xs) = (:) <$> x <*> mySequenceA xs

mapA :: (a -> Maybe b) -> ([a] -> Maybe [b])
mapA f = mySequenceA . map f

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n f = fmap (replicate n) f
