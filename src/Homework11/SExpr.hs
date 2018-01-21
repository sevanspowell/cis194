{- CIS 194 HW 11
   due Monday, 8 April
-}

module Homework11.SExpr where

import Homework11.AParser
import Control.Applicative

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p = undefined

-- oneOrMore :: Parser a -> Parser [a]
-- oneOrMore p = undefined

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

(*>) :: Applicative f => f a -> f b -> f b
-- (*>) a b = flip const <$> a <*> b
(*>) = liftA2 (flip const)

mySequenceA :: Applicative f => [f a] -> f [a]
mySequenceA [] = pure []
mySequenceA (x:xs) = (:) <$> x <*> mySequenceA xs

mapA :: (a -> Maybe b) -> ([a] -> Maybe [b])
mapA f = mySequenceA . map f

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n f = fmap (replicate n) f

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = ((:) <$> p <*> zeroOrMore p) <|> ((\_ -> []) <$> p)
-- zeroOrMore p = (++) <$> (((:) []) <$> p) <*> (((:) []) <$> p)
  -- where
  --   go = Parser $ (\str -> case runParser p str of
  --                             Nothing -> Just ([], str)
  --                             Just (a, str') -> Just ([a], str'))

-- zeroOrMore p = go
--   where
--     go = Parser $ (\str -> case runParser p str of
--                               Nothing -> Just ([], str)
--                               Just (a, str') -> (\(a1, _) (a2, s2) -> (a1 ++ a2, s2)) <$> Just ([a], str') <*> runParser (zeroOrMore p) str')


-- oneOrMore :: Parser a -> Parser [a]
-- oneOrMore p = go
--   where
--     go = Parser $ (\str -> case runParser p str of
--                               Nothing -> Nothing
--                               Just (a, str') -> (\(a1, _) (a2, s2) -> (a1 ++ a2, s2)) <$> Just ([a], str') <*> runParser (zeroOrMore p) str')
