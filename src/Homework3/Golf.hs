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
-- 119 chars
localMaxima (x : y : z : rest) = (if y > x && y > z then [y] else []) ++ localMaxima (y : z : rest)
localMaxima (_) = []

-- localMaxima' :: [Integer] -> [Integer]
-- localMaxima' xs = foldMap (\(x, y, z) -> if y > x && y > z then [y] else [])
--                   $ slidingWindow3 xs

-- localMaxima'' :: [Integer] -> [Integer]
-- localMaxima'' = map sel2 . filter isLocalMaxima . slidingWindow3
--   where
--     isLocalMaxima :: (Ord a) => (a, a, a) -> Bool
--     isLocalMaxima (a, b, c) = b > a && b > c

--     sel2 :: (a, a, a) -> a
--     sel2 (_, a, _) = a

slidingWindow3 :: [a] -> [(a, a, a)]
slidingWindow3 ys = zip3 ys (drop 1 ys) (drop 2 ys)

localMaxima' :: [Integer] -> [Integer]
localMaxima' = map (\(_, a, _) -> a) . filter (\(a, b, c) -> b > a && b > c) . slidingWindow3

histogram :: [Integer] -> String
histogram xs = (graph $ bin xs domain) ++ axis domain
  where
    domain :: [Integer]
    domain = [0..9]

    -- Sort list of integers from [0..9] into bins for each integer from [0..9],
    -- showing the number of times the integer occured in the list.
    bin :: [Integer] -> [Integer] -> [(Integer, Int)]
    -- For all digits from 0 to 9, get a new list with all the elements of the
    -- original list that have the value n (where n is a digit from 0 to 9) and find
    -- the length of that list. Next, place this in a bin.
    bin ys dom = map (\n -> (n, length $ filter ((==) n) ys)) dom

    -- Generate an x-axis for the histogram given a range of values for x
    axis :: [Integer] -> String
    -- Map each element in the domain to a "=" and reduce the expressions using
    -- concatenation (mappend of the String Monoid instance), using mempty (""
    -- for the String Monoid instance) as the original value.
    axis dom = foldMap (\_ -> "=") dom    ++ "\n"
    -- Do the same process again but instead map to the string representation of
    -- the given domain element using 'show'.
               ++ foldMap show dom        ++ "\n"

    -- Generate the graph for the histogram
    graph :: [(Integer, Int)] -> String
    -- Generate one line of dots for each element in the list [1..maxFreq], use
    -- foldr to reduce begining at the back of the list so that graph is correct
    -- way up.
    --
    -- The reducing function takes the current freq that we are generating dots
    -- for and the current string, appending to the end of the current string:
    --  A list of spaces and dots, one space or dot for each element in the
    --  bins. A dot is chosen if the freq of the current bin is greater than or
    --  equal to the current freq, otherwise an empty space is chosen. Each of
    --  these forms a line in the graph.
    graph bins = foldr (\f s -> s ++ foldMap (\x -> if snd x >= f then "*" else " ") bins ++ "\n") "" [1..maxFreq]
      where
        -- The maximum frequency is found by folding over the bins and choosing
        -- the maximum of the current max frequency and the current bin
        -- frequency.
        maxFreq :: Int
        maxFreq = foldr (\a b -> max (snd a) b) 0 bins
