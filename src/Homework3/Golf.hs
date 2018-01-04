{-# OPTIONS_GHC -Wall #-}

module Homework3.Golf where

import           Data.List (zip)

-- Given a list of elements, output a list of lists, where every nth list in the
-- output list contains every nth element from the input list.
--
-- For example, skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips :: [a] -> [[a]]
-- List elements are numbered from 1 to length. For each element in the list,
-- every n elements are grabbed from the original list, where n is the number
-- given to the element of the list currently being operated on.
--
-- "ABCD"
-- -> [(1, "A"), (2, "B"), (3, "C"), (4, "D")]
-- -> [every 1 from "ABCD", every 2 from "ABCD", ...]
skips xs = map (\(a, _) -> every a xs) $ zip [1..] xs
  where
    -- Choose every n elements from the given list, returning a new list with
    -- only those elements
    every :: Integer -> [a] -> [a]
    every n = map snd                             -- Get original list element
              . filter (\(a, _) -> mod a n == 0)  -- Only grab those elements in list where it's number is evenly divisible by the given n
              . zip [1..]                         -- Give each element in list a number starting at 1

-- Given a list of integers, find the local maxima in the input list and returns
-- them in order
localMaxima :: [Integer] -> [Integer]
-- Grab first 3 elements, if second element is a local maxima, add to list, else
-- don't. Then recursively call function on the original list, minus the first
-- element
localMaxima (x : y : z : rest) = (if y > x && y > z
                                  then [y]
                                  else [])
                                 ++ localMaxima (y : z : rest)
localMaxima (_) = []

histogram :: [Integer] -> String
histogram xs = (graph $ bin xs) ++ axis [0..9]
  where
    -- Sort list of integers from [0..9] into bins for each integer from [0..9],
    -- showing the number of times the integer occured in the list.
    bin :: [Integer] -> [(Integer, Int)]
    -- For all digits from 0 to 9, get a new list with all the elements of the
    -- original list that have the value n (where n is a digit from 0 to 9) and find
    -- the length of that list. Next, place this in a bin.
    bin ys = map (\n -> (n, length $ filter ((==) n) ys)) [0..9]

    axis :: [Integer] -> String
    axis ys = foldMap (\_ -> "=") ys    ++ "\n"
              ++ foldMap show ys        ++ "\n"

    graph :: [(Integer, Int)] -> String
    graph ys = foldr (\f s -> s ++ foldMap (\x -> if snd x >= f then "*" else " ") ys ++ "\n") "" [1..maxFreq]
      where
        maxFreq :: Int
        maxFreq = foldr (\a b -> max (snd a) b) 0 ys
