{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Homework6.Fibonacii where

import Data.List

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n > 1 = fib (n - 1) + fib (n - 2)
  | n < 0 = fib (n + 2) - fib (n + 1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Integer]
-- scanl continually applies a function to a list:
-- scanl f z [x1, x2, ...] == [z, f z x1, f (f z x1) x2, ...]
fibs2 = scanl (+) 0 (1 : fibs2)

fibs3 :: [Integer]
-- unfoldr builds a list from a seed value. The provided func is of the type:
-- (b -> Maybe(a, b)), a is prepended to the list and b is used as the next
-- element in a recursive call. Nothing can be used to finish building the list,
-- since we are generating an infinite list we do not use this.
fibs3 = unfoldr (\(a,b) -> Just (a,(b,a+b))) (0,1)

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show :: Stream a -> String
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = Cons a (Cons b (interleaveStreams as bs))

ruler :: Stream Integer
-- Because of repeating patterns of zeroes and ones, can just interleave streams
-- of these numbers with a stream of numbers that don't follow a repeating
-- pattern.
ruler = interleaveStreams zeroes (interleaveStreams ones rulerPattern)
  where
    zeroes :: Stream Integer
    -- For n = 1, 3, 5, 7, ...
    zeroes = streamRepeat 0

    ones :: Stream Integer
    -- For n = 2, 6, 10, 14, ... (alternate multiples of two)
    ones = streamRepeat 1

    rulerPattern :: Stream Integer
    -- First generate n (alternate multiples of two starting from four),
    -- skipping any we've included in the streams of ones and zeroes.
    -- Then map n to the desired number using the numUntilOdd function.
    rulerPattern = streamMap numUntilOdd $ streamFromSeed (+ 4) 4

    -- Returns the number of divisions by 2 that can be applied to the given integer
    -- before it becomes an odd number.
    numUntilOdd :: Integer -> Integer
    numUntilOdd n
      | odd n = 0
      | otherwise = 1 + numUntilOdd (n `div` 2)

-- Exercise 6
x :: Stream Integer
x = as
  where
    as :: Stream Integer
    as = (Cons 0 (Cons 1 (streamRepeat 0)))

instance Num (Stream Integer) where
  fromInteger :: Integer -> Stream Integer
  fromInteger n = Cons n (streamRepeat 0)

  negate :: Stream Integer -> Stream Integer
  negate (Cons a as) = Cons (-a) (negate as)

  (+) :: Stream Integer -> Stream Integer -> Stream Integer
  (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)

  (*) :: Stream Integer -> Stream Integer -> Stream Integer
  (*) (Cons a as) b'@(Cons b bs) = Cons (a * b) ((fromInteger a)*bs + as*b')

instance Fractional (Stream Integer) where
  -- http://www.cs.dartmouth.edu/~doug/powser.html
  (/) :: Stream Integer -> Stream Integer -> Stream Integer
  (/) (Cons a as) (Cons b bs) = qs
    where
      qs = Cons (a `div` b) (fromInteger(1 `div` b)*(as-qs*bs))

fibs4 :: Stream Integer
fibs4 = x / (1 - x - x*x)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer

instance Num (Matrix) where
  fromInteger :: Integer -> Matrix
  fromInteger n = Matrix n 0 0 n
  
  (*) :: Matrix -> Matrix -> Matrix
  (*) (Matrix a b c d) (Matrix a' b' c' d') =
    Matrix (a*a' + b*c') (a*b' + b*d') (c*a' + d*c') (c*b' + d*d')

instance Show (Matrix) where
  show :: Matrix -> String
  show (Matrix a b c d) = "(" ++ show a
                          ++ ", " ++ show b
                          ++ ", " ++ show c
                          ++ ", " ++ show d ++ ")"

fibs5 :: Integer -> Integer
fibs5 0 = 0
fibs5 n = getFn $ (Matrix 1 1 1 0)^(n - 1)
  where
    getFn :: Matrix -> Integer
    getFn (Matrix a _ _ _) = a
