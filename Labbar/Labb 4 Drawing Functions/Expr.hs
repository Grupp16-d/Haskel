module Expr where

import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Char

-- | ------------------------- Part I --------------------------------|--
-- Assignment A
-------------------------------------------------------------------------
-- Recursive data types fro sin, cos, addision, multiplication String
--
data Expr = Num Double
          | Var
          | Add Expr Expr
          | Mul Expr Expr
          | Cos Expr
          | Sin Expr
    deriving (Eq, Show)


--instance Show Expr where
  --show = showExpr

ex1 = Cos (Add (Num 0.5) (Mul (Num 0.5) Var))
ex2 = Add (Mul (Num 2)(Num 3))(Mul (Num 4)(Num 5))
ex3 = Sin Var
ex4 = Sin (Cos Var)
ex5 = Add (Sin Var) (Cos Var)
ex6 = Add (Add Var (Num 1)) Var

-- Assignment B
-------------------------------------------------------------------------
-- A function that converts any expression to a string
showExpr :: Expr -> String
showExpr expr = case expr of
    (Num n)     -> show n
    Var         -> "x"
    (Add e1 e2) -> showExpr e1 ++ " + " ++ showExpr e2
    (Mul e1 e2) -> showFactor e1 ++ " * " ++ showFactor e2
    (Cos e)     -> "cos " ++ showTrig e
    (Sin e)     -> "sin " ++ showTrig e
  where
    showFactor (Add a b) = '(':showExpr (Add a b) ++ ")"
    showFactor e         = showExpr e
    showTrig e = case e of
      (Mul a b) -> '(':showExpr (Mul a b) ++ ")"
      (Add a b) -> '(':showExpr (Add a b) ++ ")"
      e         -> showExpr e

-- Assigment C
-------------------------------------------------------------------------
-- A function that, given an expression, and the value for the variable x,
-- calculates the value of the expression.
eval :: Expr -> Double -> Double
eval (Num n)     valX = n
eval Var         valX = valX
eval (Add e1 e2) valX = eval e1 valX + eval e2 valX
eval (Mul e1 e2) valX = eval e1 valX * eval e2 valX
eval (Cos e1)    valX = cos(eval e1 valX)
eval (Sin e1)    valX = sin(eval e1 valX)

-- Assigment D
-------------------------------------------------------------------------
--
type Parser a = String -> Maybe (a, String)
-- Given a string returns Just Expr if the string is on Expr
-- Expession = sum     of terms
-- terms     = product of factors
-- factors   = trigExpr or expression in brackets or an number or a Variable
-- trigExpr  = are the trigExpr applied to a factors
readExpr :: String -> Maybe Expr
readExpr s = case expr (filter (not . isSpace) s) of
    Just(e, "") -> Just e
    _           -> Nothing
  where  
    expr   = chain term '+' Add
    term   = chain factor '*' Mul

    factor ('(':s) = case expr s of
                          Just (e, ')':s1) -> Just (e,s1)
                          _                -> Nothing
    factor ('s':'i':'n':s) = case factor s of
        Just (e, s1) -> Just (Sin e, s1)
        _            -> Nothing
    factor ('c':'o':'s':s) = case factor s of 
        Just (e, s1) -> Just (Cos e, s1)
        _            -> Nothing
    factor ('x':s)         = Just (Var, s)    
    factor s               = num s
    num s = case reads s :: [(Double, String)] of
        [] -> Nothing
        _  -> case Just . head . reads $ s :: Maybe (Double, String) of
                Just (n,s1) -> Just (Num n, s1)
                _           -> Nothing


-- A chain of ps, separated by the op character,
-- and with results combined using f
chain :: Parser a -> Char -> (a -> a -> a) ->  Parser a
chain p op f s = case p s of
  Just (ne,c:s1)
           |c == op -> case chain p op f s1 of
                               Just(e,s2) -> Just (f ne e, s2)
                               Nothing    -> Just (ne,c:s1)
  r               -> r
--Assigment E
-------------------------------------------------------------------------
-- A property that checks if an expression
-- using the functions showExpr and readExpr
-- produce "the same" result as the expression you started with.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr exp = (readExpr . showExpr $ exp) == (Just . assoc $ exp)


rExp :: Int -> Gen Expr
rExp s = frequency [(1,rNum), (1, return Var) ,(s,rBin), (s', rTrig)]
  where 
   rNum = do
     n <- arbitrary
     return $ Num n
   rTrig = do
     func <- elements [Sin, Cos]
     e1   <- rExp s'
     return $ func e1
   rBin = do
     op <- elements [Add,Mul]
     e1 <- rExp s'
     e2 <- rExp s'
     return $ op e1 e2
     
   s' = s `div` 2 
  
instance Arbitrary Expr
  where arbitrary = sized rExp

assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3) = assoc $ Add e1 (Add e2 e3)
assoc (Mul (Mul e1 e2) e3) = assoc $ Mul e1 (Mul e2 e3)
assoc (Mul e1 e2)          = Mul (assoc e1) (assoc e2) 
assoc (Add e1 e2)          = Add (assoc e1) (assoc e2)
assoc (Sin e1)             = Sin (assoc e1)
assoc (Cos e1)             = Cos (assoc e1)
assoc e                    = e
