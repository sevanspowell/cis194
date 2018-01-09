{-# LANGUAGE InstanceSigs #-}
module Homework6.Fibonacci where

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
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a xs) = Cons (f a) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- interleaveStreams :: Stream a -> Stream a -> Stream a

zeroes :: Stream Integer
zeroes = streamRepeat 0

ones :: Stream Integer
ones = streamRepeat 1

sumsOfTwo :: Stream Integer
sumsOfTwo = streamFromSeed (+ 2) 2
