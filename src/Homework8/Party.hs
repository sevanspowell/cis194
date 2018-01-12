{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Homework8.Party where

import Data.Tree

import Homework8.Employee

-- Exercise 1

-- | Add an Employee to a GuestList and recalculate the fun score on the
--   GuestList. Note that this is a naive calculation, it merely sums the fun
--   factors of each employee in the list.
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL gl fun) = GL ([emp] ++ gl) (fun + empFun emp)

instance Monoid (GuestList) where
  mempty = GL [] 0
  mappend (GL gl1 fun1) (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ funA) b@(GL _ funB) = if funA > funB
                                      then a
                                      else b

-- Exercise 2

-- http://hackage.haskell.org/package/containers-0.5.10.2/docs/src/Data.Tree.html#foldTree
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f = go where
  -- go :: Tree a -> b
  go (Node x ts) = f x (map go ts)

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [] = (GL [emp] (empFun emp), GL [] 0)
nextLevel emp xs = (glCons emp $ computeBest moreFunGivenBossPresent xs, computeBest moreFun xs)
  where
    -- Fold over list and find GuestList with maximum fun using given maximum
    -- function to resolve "funness".
    computeBest :: (GuestList -> GuestList -> GuestList) -> [(GuestList, GuestList)] -> GuestList
    computeBest maxFunc = foldMap (\(w, wo) -> maxFunc w wo) --foldr (\(w, wo) currMax -> maxFunc (maxFunc currMax wo) w) (maxFunc with without) xs

    -- Return the guest list with the most fun given that the root's immediate
    -- boss is present. It is assumed that the "without" list of the subtree is
    -- passed as the second argument to the function.
    moreFunGivenBossPresent :: GuestList -> GuestList -> GuestList
    moreFunGivenBossPresent _ wo = wo

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun (fst results) (snd results)
  where
    results = treeFold nextLevel t

-- Exercise 5

main :: IO ()
main = do
  putStrLn "Please enter a company hierarchy file to read: "
  file <- readLn
  str <- readFile file
  putStrLn . show . maxFun . read $ str
