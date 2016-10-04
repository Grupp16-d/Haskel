-- Föreläsning 9 2016 - 30 - 09
-- Make a Game where you shall calculate the value of a symbolic expression

module Symbolic where

import Test.QuickCheck
import Data.List
import Data.Maybe

data Expr 
    = Num Integer
    | Var VarName
    | Add Expr Expr
    | Mul Expr Expr
    deriving Eq

type VarName = String

ex1 = Mul (Add (Var "y") (Num 2)) (Var "x")
ex2 = Num (-5) `Add` (Num 2 `Mul` Num 4)

showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Var v)     = v
showExpr (Add e1 e2) = showExpr e1   ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
    where showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
          showFactor e           = showExpr e

-- Gives a list of all the varialbe names
vars :: Expr -> [VarName]
vars (Num n)     = []
vars (Var x)     = [x]
vars (Add e1 e2) = vars e1 ++ vars e2
vars (Mul e1 e2) = vars e1 ++ vars e2

-- Calculates the value of Expr
type Tabel = [(VarName,Integer)]
eval :: Tabel -> Expr -> Integer
eval t (Num n)     = n 
eval t (Var x)     = fromJust $ lookup x t
eval t (Add e1 e2) = eval t e1 + eval t e2 
eval t (Mul e1 e2) = eval t e1 * eval t e2

extable' = [("x",2),("y",3),("z",4)]


instance Arbitrary Expr
    where arbitrary = sized rExp 
rExp :: Int -> Gen Expr
rExp s = frequency [(1,rNum),(1,rVar),(s,rBin)]
    where 
      rVar = elements [Var "x",Var "y",Var "z"]
      rNum = do
        n <- arbitrary
        return $ Num n
      -- fmap Num arbitrary       
      rBin = do
        op <- elements [Add, Mul]
        e1 <- rExp s'
        e2 <- rExp s'
        return $ op e1 e2
      s' = s `div` 2  
      
derive :: VarName -> Expr -> Expr
-- derive x e
-- the derivateive of e wiht respect to x (d/dx)

derive x (Add e1 e2) = Add (derive x e1) (derive x e2)
derive x (Mul e1 e2) = Add (Mul (derive x e1) e2) (Mul e1 (derive x e2))
derive x (Var y)
    | x == y         = Num 1
    | otherwise      = Num 0
derive x (Num n)     = Num 0





