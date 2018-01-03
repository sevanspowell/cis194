{-# OPTIONS_GHC -Wall #-}

module Homework2.LogAnalysis where

import           Homework2.Log

-- Produce a LogMessage from a log line.
parseMessage :: String -> LogMessage
parseMessage = parseLogMessage . words
  where
    parseLogMessage :: [String] -> LogMessage
    parseLogMessage ("I" : timestamp : msg) =
      LogMessage Info (read timestamp) (unwords msg)
    parseLogMessage ("W" : timestamp : msg) =
      LogMessage Warning (read timestamp) (unwords msg)
    parseLogMessage ("E" : level : timestamp : msg) =
      LogMessage (Error $ read level) (read timestamp) (unwords msg)
    parseLogMessage arr =
      Unknown (unwords arr)

-- Produce an array of log messages from a log file.
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Given a log message and a sorted message tree, return a sorted message tree
-- containing the given log message.
insert :: LogMessage -> MessageTree -> MessageTree

-- Return original tree unchanged if unknown log message
insert (Unknown _) tree = tree

-- Add message to tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ insertTime _)
       (Node left value@(LogMessage _ nodeTime _) right)
  | insertTime < nodeTime = Node (insert msg left) value right
  | insertTime > nodeTime = Node left value (insert msg right)
  | otherwise             = Node left msg right

-- Do nothing if found "unknown" log message in tree, tree should not contain
-- these values
insert (LogMessage _ _ _) tree@(Node _ (Unknown _) _) = tree


build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg1 : rest) = insert msg1 (build rest)

-- Given a sorted message tree, provides a list of all messages it contains,
-- sorted by timestamp ascending
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

-- Given an unsorted list of LogMessages, returns a list of messages
-- corresponding to errors with a severity of 50 or greater
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMsg . inOrder . build . filter (severity 50)
  where 
    severity :: Int -> LogMessage -> Bool
    severity n (LogMessage (Error level) _ _) = level >= n
    severity _ _ = False

    extractMsg :: [LogMessage] -> [String]
    extractMsg [] = []
    extractMsg ((LogMessage _ _ msg) : rest) = [msg] ++ extractMsg rest
    extractMsg ((Unknown msg) : rest) = [msg] ++ extractMsg rest
