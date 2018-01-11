module Homework7.JoinListBufEditor where

import Homework7.JoinList
import Homework7.Sized
import Homework7.Scrabble
import Homework7.Buffer
import Homework7.Editor

main = runEditor editor
  $ (fromString :: String -> JoinList (Score, Size) String)
  $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
