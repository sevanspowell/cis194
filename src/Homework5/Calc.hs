{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Homework5.Calc where

import Homework5.ExprT as ExprT
import Homework5.Parser
import Homework5.StackVM as VM

-- Exercise 1

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp ExprT.Lit ExprT.Add ExprT.Mul
  where
    evalMaybe :: Maybe ExprT -> Maybe Integer
    evalMaybe (Just expr) = Just (eval expr)
    evalMaybe Nothing = Nothing

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit :: Integer -> ExprT
  lit = ExprT.Lit

  add :: ExprT -> ExprT -> ExprT
  add = ExprT.Add

  mul :: ExprT -> ExprT -> ExprT
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
  lit :: Integer -> Integer
  lit = id

  add :: Integer -> Integer -> Integer
  add = (+)

  mul :: Integer -> Integer -> Integer
  mul = (*)

instance Expr Bool where
  lit :: Integer -> Bool
  lit x
    | x > 0 = True
    | otherwise = False

  add :: Bool -> Bool -> Bool
  add = (||)

  mul :: Bool -> Bool -> Bool
  mul = (&&)

newtype MinMax = MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit :: Integer -> MinMax
  lit = MinMax

  add :: MinMax -> MinMax -> MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b

  mul :: MinMax -> MinMax -> MinMax
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit :: Integer -> Mod7
  lit = Mod7 . flip mod 7

  add :: Mod7 -> Mod7 -> Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7

  mul :: Mod7 -> Mod7 -> Mod7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr VM.Program where
  lit :: Integer -> VM.Program
  lit a = (++) [] $ [PushI a]

  add :: VM.Program -> VM.Program -> VM.Program
  add a b = a ++ b ++ [VM.Add]

  mul :: VM.Program -> VM.Program -> VM.Program
  mul a b = a ++ b ++ [VM.Mul]

parseProgram :: String -> Maybe Program
parseProgram = parseExp lit add mul

compile :: String -> Maybe Program
compile = parseProgram

execute :: String -> Either String StackVal
execute = run . compile
  where
    run :: Maybe Program -> Either String StackVal
    run (Just program) = stackVM program
    run (Nothing) = Left $ "No program provided."
