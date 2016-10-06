module Expr where

import Test.QuickCheck
import Data.List
import Data.Maybe

-- | ------------------------- Part I --------------------------------|--
-- Assignment A
-------------------------------------------------------------------------
-- Recursive data types fro sin, cos, addision, multiplication String
--
data Expr = Num Double
          | Var VarName
          | Add Expr Expr
          | Mul Expr Expr
          | Cos Expr
          | Sin Expr
    deriving Eq

type VarName = String

instance Show Expr where
    show = showExpr

ex1 = Cos (Add (Num 0.5) (Mul (Num 0.5) (Var "x")))

-- Assignment B
-------------------------------------------------------------------------
-- A function that converts any expression to a string
showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Var x)     = x
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Cos e)     = "Cos " ++ showTrig e
showExpr (Sin e)     = "Sin " ++ showTrig e

-- Needed these as toplevel functions becuse where  did not work as intended
showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e
showTrig :: Expr -> String
showTrig (Var x) = x
showTrig e       = "(" ++ showExpr e ++ ")"

-- Assigment C
-------------------------------------------------------------------------
-- A function that, given an expression, and the value for the variable x,
-- calculates the value of the expression.
eval :: Expr -> Double -> Double
eval (Num n)     valX = n
eval (Var x)     valX = valX
eval (Add e1 e2) valX = eval e1 valX + eval e2 valX
eval (Mul e1 e2) valX = eval e1 valX * eval e2 valX
eval (Cos e1)    valX = cos(eval e1 valX)
eval (Sin e1)    valX = sin(eval e1 valX)

-- Assigment D
-------------------------------------------------------------------------
--
readExpr :: String -> Maybe Expr
readExpr = undefined

--Assigment E
-------------------------------------------------------------------------
-- A property that checks if an expression
-- using the functions showExpr and readExpr)
-- produce "the same" result as the expression you started with.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined