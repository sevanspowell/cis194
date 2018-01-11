{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Homework7.JoinList where

import Data.Monoid

import Homework7.Sized
import Homework7.Scrabble

-- Exercise 1

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

instance Monoid m => Monoid (JoinList m a) where
  mempty :: JoinList m a
  mempty = Empty

  mappend :: JoinList m a -> JoinList m a -> JoinList m a
  mappend = (+++)

-- | Return the annotation at the root of the JoinList.
tag :: Monoid m => JoinList m a -> m
tag (Empty)        = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- | Given two JoinLists, append them to form a new JoinList, whose monoidal
--   annotation is derived from the two given JoinLists.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty a@(_) = a 
(+++) a@(_) Empty = a
(+++) a@(_) b@(_) = Append (tag a `mappend` tag b) a b

-- Exercise 2

-- Example tree
-- let t = (Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h'))
sizeAnnot :: (Sized b) => JoinList b a -> Int
sizeAnnot Empty = 0
sizeAnnot (Single b _) = getSize $ size b
sizeAnnot (Append b _ _) = getSize $ size b

-- | Find the element of the JoinList at the given index. THe JoinList should
--   use it's monoidal annotation to record the size of (number of elements in)
--   each subtree.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i curr@(Append _ l1 l2)
  | i < 0         = Nothing
  | i >= sizeCurr = Nothing
  | i < sizeL1    = indexJ i l1
  | otherwise     = indexJ (i - sizeL1) l2
  where
    sizeCurr = sizeAnnot curr
    sizeL1   = sizeAnnot l1

-- | Drop the first n elements from a JoinList.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n a | n <= 0 = a
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ n curr@(Append _ l1 l2)
  | n >= sizeCurr = Empty
  | n >= sizeL1   = dropJ (n - sizeL1) l2
  | otherwise     = Append (tag newL1 <> tag l2) newL1 l2
  where
    sizeCurr = sizeAnnot curr
    sizeL1 = sizeAnnot l1
    newL1  = dropJ n l1

-- | Return the first n elements of a JoinList, dropping all other elements.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ a@(Single _ _) = a
takeJ n curr@(Append _ l1 l2)
  | n >= sizeCurr = curr
  | n >  sizeL1   = Append (tag l1 <> tag newL2) l1 newL2
  | otherwise     = (takeJ n l1)
  where
    sizeCurr = sizeAnnot curr
    sizeL1   = sizeAnnot l1 
    newL2    = takeJ (n - sizeAnnot l1) l2

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine str = Single (scoreString str) str
