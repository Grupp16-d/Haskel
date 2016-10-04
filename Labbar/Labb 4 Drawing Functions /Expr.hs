module Expr where

import Test.QuickCheck
import Data.List
import Data.List.Split

-- | ------------------------- Part I --------------------------------|--
-- Assignment A
-------------------------------------------------------------------------
--
data Expr = Num Double
          | Var String
          | Add Expr Expr
          | Mul Expr Expr
          | Cos Expr
          | Sin Expr
        
    deriving Eq

instance Show Expr where
	show = showExpr

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

showFactor (Add a b) = "(" ++ showExpr (Add a b) ++ ")" 
showFactor e = showExpr e
showTrig (Var x) = x
showTrig e = "(" ++ showExpr e ++ ")"


--Assigment C
-------------------------------------------------------------------------
--
eval :: Expr -> Double -> Double
eval = undefined

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