{- CIS 194 HW 11
   due Monday, 8 April
-}

module Homework11.SExpr where

import           Control.Applicative
import           Data.Char           (isAlphaNum, isAlpha, isSpace)
import           Homework11.AParser

------------------------------------------------------------
--  0. Applicative tools
------------------------------------------------------------

-- (*>) :: Applicative f => f a -> f b -> f b
-- (*>) a b = flip const <$> a <*> b
-- (*>) = liftA2 (flip const)

mySequenceA :: Applicative f => [f a] -> f [a]
mySequenceA []     = pure []
mySequenceA (x:xs) = (:) <$> x <*> mySequenceA xs

mapA :: (a -> Maybe b) -> ([a] -> Maybe [b])
mapA f = mySequenceA . map f

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n f = fmap (replicate n) f

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = ((\a -> (++) [a]) <$> p <*> zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

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

parseAtom :: Parser Atom
parseAtom =  (N <$> (spaces *> posInt)) <|> (I <$> (spaces *> ident))

parseSExpr :: Parser SExpr
parseSExpr = (A <$> parseAtom)
  <|> (Comb <$> (openParen *> oneOrMore parseSExpr <* closeParen))
  where
    openParen :: Parser Char
    openParen = spaces *> satisfy (== '(') <* spaces

    closeParen :: Parser Char
    closeParen = spaces *> satisfy (== ')') <* spaces
