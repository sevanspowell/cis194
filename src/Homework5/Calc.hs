{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}

module Homework5.Calc where

import qualified Homework5.ExprT as ExprT
import Homework5.Parser
import qualified Homework5.StackVM as VM
-- import qualified Data.Map as M

-- Exercise 1

eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp ExprT.Lit ExprT.Add ExprT.Mul
  where
    evalMaybe :: Maybe ExprT.ExprT -> Maybe Integer
    evalMaybe (Just expr) = Just (eval expr)
    evalMaybe Nothing = Nothing

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit :: Integer -> ExprT.ExprT
  lit = ExprT.Lit

  add :: ExprT.ExprT -> ExprT.ExprT -> ExprT.ExprT
  add = ExprT.Add

  mul :: ExprT.ExprT -> ExprT.ExprT -> ExprT.ExprT
  mul = ExprT.Mul

reify :: ExprT.ExprT -> ExprT.ExprT
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
  lit a = (++) [] $ [VM.PushI a]

  add :: VM.Program -> VM.Program -> VM.Program
  add a b = a ++ b ++ [VM.Add]

  mul :: VM.Program -> VM.Program -> VM.Program
  mul a b = a ++ b ++ [VM.Mul]

parseProgram :: String -> Maybe VM.Program
parseProgram = parseExp lit add mul

compile :: String -> Maybe VM.Program
compile = parseProgram

execute :: String -> Either String VM.StackVal
execute = run . compile
  where
    run :: Maybe VM.Program -> Either String VM.StackVal
    run (Just program) = VM.stackVM program
    run (Nothing) = Left $ "No program provided."

-- Exercise 6

-- class HasVars a where
--   var :: String -> a

-- data VarExprT = Lit Integer
--            | Add VarExprT VarExprT
--            | Mul VarExprT VarExprT
--            | Var String
--   deriving (Show, Eq)

-- instance Expr VarExprT where
--   lit :: Integer -> VarExprT
--   lit = Lit

--   add :: VarExprT -> VarExprT -> VarExprT
--   add = Add

--   mul :: VarExprT -> VarExprT -> VarExprT
--   mul = Mul

-- instance HasVars VarExprT where
--   var :: String -> VarExprT
--   var = Var

-- instance HasVars (M.Map String Integer -> Maybe Integer)
--   var :: String -> (M.Map String Integer -> Maybe Integer)
--   var key = (\map -> find map key)

-- instance Expr (M.Map String Integer -> Maybe Integer)
--   lit :: Integer -> (M.Map String Integer -> Maybe Integer)
--   lit x = (\_ -> Just x)

--   add :: (M.Map String Integer -> Maybe Integer)
--          -> (M.Map String Integer -> Maybe Integer)
--          -> (M.Map String Integer -> Maybe Integer)
--   add _ (Just a) _ (Just b) = (\map -> Just (a + b))
--   add _ (Nothing) _ (Nothing) = (\map -> Nothing)

--   mul :: (M.Map String Integer -> Maybe Integer)
--          -> (M.Map String Integer -> Maybe Integer)
--          -> (M.Map String Integer -> Maybe Integer)
--   mul _ (Just a) _ (Just b) = (\map -> Just (a * b))
--   mul _ (Nothing) _ (Nothing) = (\map -> Nothing)
