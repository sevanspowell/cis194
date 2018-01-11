{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Homework7.JoinList where

import Homework7.Sized

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

getL :: Monoid m => JoinList m a -> JoinList m a
getL (Empty)        = Empty
getL a@(Single _ _)   = a
getL (Append _ l _) = l 

-- | Given two JoinLists, append them to form a new JoinList, whose monoidal
--   annotation is derived from the two given JoinLists.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty a@(_) = a 
(+++) a@(_) Empty = a
(+++) a@(_) b@(_) = Append (tag a `mappend` tag b) a b

-- Exercise 2

-- | Find the element of the JoinList at the given index. THe JoinList should
--   use it's monoidal annotation to record the size of (number of elements in)
--   each subtree.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append m l1 l2)
  | i < 0                       = Nothing
  | i >= getSize (size m)       = Nothing
  | i < getSize (size $ tag l1) = indexJ i l1
  | otherwise                   = indexJ (i - (getSize (size $ tag l1))) l2

-- Example tree
-- let t = (Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h'))
