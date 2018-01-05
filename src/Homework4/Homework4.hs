{-# OPTIONS_GHC -Wall #-}

module Homework4.Homework4 where

--
-- Exercise 1
--

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map multiplier
  where
    multiplier :: Integer -> Integer
    multiplier x = if even x
                   then (x - 2)
                   else 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = foldr (+) 0 . filter even . takeWhile (/= 1) . iterate f
  where
    f :: Integer -> Integer
    f 1 = 0
    f n = if even n
          then n `div` 2
          else 3 * n + 1

--
-- Exercise 2
--

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Given an element to insert into a tree and a tree, return a new tree with the
-- given element inserted. The insertion is guaranteed to keep the tree
-- balanced.
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n l val r)
  -- Insert the element into the branch that has a smaller height
  | hl < hr   = Node n         ntl val r
  | hl > hr   = Node n         l   val ntr
  -- If the height of each branch is equal, decide which branch to insert into
  -- based on the height of the potential new branches.
  | nhl < nhr = Node n         ntl val r
  | nhl > nhr = Node n         l   val ntr
  -- If the height of the new left branch is equal to the height of the new
  -- right node, arbitrarily insert into the right branch. It is necessary to
  -- set the height of the current node as in this case inserting is guaranteed
  -- to change the height of the current node.
  | otherwise = Node (nhr + 1) l   val ntr
  where
    hl  = height l
    hr  = height r
    ntl = insert x l
    ntr = insert x r
    nhl = height ntl
    nhr = height ntr 

-- Get the height of the tree, the longest path from the root to a leaf.
height :: Tree a -> Integer
height Leaf = 0
height (Node _ Leaf _ Leaf) = 0
height (Node _ l _ r) = (if height l > height r then height l else height r) + 1

-- Will Ness
-- https://stackoverflow.com/questions/16157203/build-balanced-binary-tree-with-foldr
showTree :: (Show a) => Tree a -> String
showTree Leaf = ""
showTree n@(Node i _ _ _) = go i n
  where
  go _ (Leaf) = ""
  go j (Node _ l c r) = go (j-1) l ++
    replicate (4*fromIntegral j) ' ' ++ show c ++ "\n" ++ go (j-1) r
