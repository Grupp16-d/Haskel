module Expr where

import Test.QuickCheck
import Data.List
import Data.List.Split

-- | ------------------------- Part I --------------------------------|--
-- Assignment A
-------------------------------------------------------------------------
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

ex1 = Cos (Add (Num 0.5) (Mul (Num 0.5) (Num 1)))
-- Assignment B
-------------------------------------------------------------------------
--
showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Var x)     = x
showExpr (Add e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
showExpr (Cos e)     = "Cos " ++ showTrig e
showExpr (Sin e)     = "sin " ++ showTrig e

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")"
showFactor e         = showExpr e
showTrig :: Expr -> String
showTrig (Var x) = x
showTrig e       = "(" ++ showExpr e ++ ")"

test 35 = "funkar"
test a = test' a
 where test' a = test (a+1)
--Assigment C
-------------------------------------------------------------------------
--
eval :: Expr -> Double -> Double
eval (Num n) valX     = n
eval (Var x) valX     = valX
eval (Add e1 e2) valX = 

type Tabel = [(VarName,Integer)]
eval :: Tabel -> Expr -> Integer
eval t (Num n)     = n
eval t (Var x)     = fromJust $ lookup x t
eval t (Add e1 e2) = eval t e1 + eval t e2
eval t (Mul e1 e2) = eval t e1 * eval t e2
--Assigment D
-------------------------------------------------------------------------
--
readExpr :: String -> Maybe Expr
readExpr = undefined

--Assigment E
-------------------------------------------------------------------------
--
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined