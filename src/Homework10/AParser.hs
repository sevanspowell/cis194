{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}
{- CIS 194 HW 10
   due Monday, 1 April
-}

module Homework10.AParser where

import Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here



------------------------------------------------------------

-- Exercise 1

-- | Apply the given function to the first element of a pair only
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- Exercise 2

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  -- Follow the types:
  -- runParser :: String -> Maybe (a, String)
  -- first :: (a -> b) -> (a, String) -> (b, String)
  -- fmap :: ((a, String) -> (b, String)) -> Maybe (a, String) -> Maybe (b, String)
  -- Composing (fmap (first f)) and runParser provides the required type:
  --   String -> Maybe (b, String)
  fmap f p = Parser (fmap (first f) . runParser p)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\str -> Just (x, str))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p1 p2 =  Parser ((extractThenAp p2) . runParser p1)
    where
      -- Take a parser and the results of another parser that produces a
      -- function and chain them together, applying the second parser to what's
      -- left of the first parser and then applying the function to the final
      -- parser result.
      extractThenAp :: Parser a -> Maybe ((a -> b), String) -> Maybe (b, String)
      extractThenAp p2' (Just (f, rest)) = runParser (fmap f p2') $ rest
      extractThenAp _ Nothing = Nothing

-- Exercise 3

takeFirst :: a -> b -> a
takeFirst = const

takeLast :: a -> b -> b
takeLast = flip const

-- | Parse the character a followed by the character b and return them as a
--   pair: runParser abParser 'ab' = Just (('a', 'b'), "").
--
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser' :: Parser String
abParser' = toString <$> char 'a' <*> char 'b'
  where
    toString :: Char -> Char -> String
    toString a b = [a, b]

abParser'' :: Parser Char
abParser'' = takeLast <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = unit <$> char 'a' <*> char 'b'
  where
    unit :: a -> b -> ()
    unit _ _ = ()
    -- unit = (const (const ()))

-- | Parse a pair of ints separated by whitespace and return them
--   (e.g. runParser intPair "12 34" == Just ([12, 34])).
intPair :: Parser [Integer]
intPair = (\a b -> [a, b]) <$> (takeFirst <$> posInt <*> char ' ') <*> posInt

-- Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (\_ -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = Parser (\str -> (runParser p1 str) <|> (runParser p2 str))

-- Exercise 5

-- | Parses either an integer value or an uppercase character, fails otherwise.
intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|> (const () <$> satisfy isUpper)
