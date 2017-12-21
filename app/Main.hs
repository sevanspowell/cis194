{-# OPTIONS_GHC -Wall #-}

module Main where

import Lib
import Prelude (IO, Integral, (++), (.), div, mod)

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

-- Double every other digit starting from the first digit on the right
doubleEveryOther :: Integral a => [a] -> [a]
doubleEveryOther = map (2*) . reverse
