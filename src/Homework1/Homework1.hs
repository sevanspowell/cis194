{-# OPTIONS_GHC -Wall #-}

module Homework1.Homework1 where

import           Data.List (null, reverse)
import           Prelude   (Bool, Integer, Integral, String, concat, div, flip,
                            map, mod, quot, rem, sum, ($), (*), (++), (-), (.),
                            (==))

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

-- Validate a credit card number
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
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- Lars H
-- https://stackoverflow.com/questions/3607161/towers-of-hanoi-with-k-pegs
-- hanoi for n disks and r pegs [p1, p2, ..., pr]
hanoiR :: Integer -> [a] -> [(a, a)]

-- zero disks: no moves needed.
hanoiR 0 _ = []

-- one disk: one move and two pegs needed.
hanoiR 1 (p1 : p2 : _) = [(p1, p2)]

-- n disks and r > 3 pegs: use Frame-Stewart algorithm
hanoiR n (p1 : p2 : p3 : rest) =
    hanoiR k (p1 : p3 : p2 : rest) ++
    hanoiR (n - k) (p1 : p2 : rest) ++
    hanoiR k (p3 : p2 : p1 : rest)
    where k =
            if null rest
            then n - 1
            else n `quot` 2
          -- | null rest   = n - 1
          -- | otherwise   = n `quot` 2

hanoiR _ _ = []
