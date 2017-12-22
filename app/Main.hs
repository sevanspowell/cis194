{-# OPTIONS_GHC -Wall #-}

module Main where

import Lib
import Prelude ((-), String, (==), rem, flip, Bool, Integer, (*), IO, Integral, (++), (.), div, mod, concat, map, ($), sum)

import Data.List (reverse)

main :: IO ()
main = someFunc

--
-- Exercise 1
--

-- Get the digits of a given integer
toDigits :: Integral a => a -> [a]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

-- Decompose a given integer into a list of it's digits, reversed.
toDigitsRev :: Integral a => a -> [a]
toDigitsRev = reverse . toDigits

--
-- Exercise 2
--

-- Double every other digit starting from the second digit on the right
doubleEveryOther :: Integral a => [a] -> [a]
doubleEveryOther = reverse . everyOther (* 2) . reverse

-- Apply the given function to every other element in the list, starting from
-- the second element
everyOther :: (a -> a) -> [a] -> [a]
everyOther f (x:y:xs) = x : f y : everyOther f xs
everyOther _ (x)      = x

--
-- Exercise 3
--

-- Sum all the digits in the given list of integers
-- (e.g. [16, 2, 13] = 1 + 6 + 2 + 1 + 3 = 13)
sumDigits :: Integral a => [a] -> a
sumDigits xs = sum . concat $ map toDigits xs

--
-- Exercise 4
--

validate :: Integer -> Bool
validate = (==) 0 . flip rem 10 . sumDigits . everyOther (* 2) . toDigitsRev

--
-- Exercise 5
--

type Peg = String
type Move = (Peg, Peg)

-- Given the number of discs and names for the three pegs, returns a list of
-- moves to be performed to move the stack of hanoi discs from the first peg to
-- the second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a b c ++ [(a, b)] ++ hanoi (n - 1) c a b
