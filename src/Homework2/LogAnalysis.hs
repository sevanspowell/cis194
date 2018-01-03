
module Homework2.LogAnalysis where

import Homework2.Log

parseMessage :: String -> LogMessage
parseMessage = parseLogMessage . words 
  where
    parseLogMessage :: [String] -> LogMessage
    parseLogMessage arr@(messageType : rest) =
      case messageType of
        "I" -> parseInfoMessage    rest
        "W" -> parseWarningMessage rest
        "E" -> parseErrorMessage   rest
        _   -> Unknown (unwords arr)

    parseInfoMessage :: [String] -> LogMessage
    parseInfoMessage (timestamp : rest) = LogMessage Info (read timestamp) (unwords rest)

    parseWarningMessage :: [String] -> LogMessage
    parseWarningMessage (timestamp : rest) = LogMessage Warning (read timestamp) (unwords rest)

    parseErrorMessage :: [String] -> LogMessage
    parseErrorMessage (level : timestamp : rest) = LogMessage (Error $ read level) (read timestamp) (unwords rest)

parse :: String -> [LogMessage]
parse = map parseMessage . lines
