{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 05
--
----------------------------------------------------------------------

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20
-- True

eval :: ExprT -> Integer
eval (ExprT.Add (Lit x) (Lit y))                  = eval (ExprT.Lit (x + y))
eval (ExprT.Add a b)                              = eval a + eval b
eval (ExprT.Mul (ExprT.Lit x) (ExprT.Lit y))      = eval (ExprT.Lit (x * y))
eval (ExprT.Mul a b)                              = eval a * eval b
eval (ExprT.Lit x)                                = x

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr xs =  eval <$> parseExp ExprT.Lit ExprT.Add ExprT.Mul xs


----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

reify :: ExprT -> ExprT
reify = id

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

class Exp a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Exp ExprT where
  lit = ExprT.Lit
  add = ExprT.Add 
  mul = ExprT.Mul 

instance Exp Integer where
  lit x = x
  add = (+)
  mul = (*)

instance Exp Bool where
  lit x
    | x > 0     = True
    | otherwise = False
  add = (||)
  mul = (&&)

instance Exp MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Exp Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  
----------------------------------------------------------------------
-- Exercise 5 (do this OR exercise 6)
----------------------------------------------------------------------

instance Exp Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile x = stackVM <$> parseExp lit add mul x :: Maybe Program


----------------------------------------------------------------------
-- Exercise 6 (do this OR exercise 5)
----------------------------------------------------------------------

-- |
--
-- >>> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
