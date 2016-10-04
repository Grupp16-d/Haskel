-- Föreläsning 9 2016 - 30 - 09
-- Make a Game where you shall calculate the value of a arithmetic expression
module Arithmetic where

import Test.QuickCheck

data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
    deriving Eq
    
instance Show Expr where 
    show = showExpr

-- writeing this (1+2)*4 with the datatype Expr
ex1 = Mul (Add (Num 1) (Num 2)) (Num 4)

-- writeing this 1 + 2 * 4 with the datatype Expr
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))

-- Calculates the value of Expr
eval :: Expr -> Integer
eval (Num n)     = n 
eval (Add e1 e2) = eval e1 + eval e2 
eval (Mul e1 e2) = eval e1 * eval e2

-- writes the Expr as a mathematical expression
showExpr :: Expr -> String
showExpr (Num n)     = show n
showExpr (Add e1 e2) = showExpr e1   ++ " + " ++ showExpr e2
showExpr (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2
    where showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")"
          showFactor e           = showExpr e

-- Generate a random Expr for quickCheck
-- :t sized :: (Int -> Gen a) -> Gen a
-- sized takes a size dependant Generator and gives back a Generators
instance Arbitrary Expr
    where arbitrary = sized rExp 

rExp :: Int -> Gen Expr
rExp s = frequency [(1,rNum),(s,rBin)]
    where 
      rNum = do
        n <- arbitrary
        return $ Num n
      rBin = do
        op <- elements [Add, Mul]
        e1 <- rExp s'
        e2 <- rExp s'
        return $ op e1 e2
      s' = s `div` 2  
          
-- The game part
-- :t sample' :: Gen a -> IO [a]
difficulty = 3 -- instead of haveing random 
               -- numbers in your programs its better to define a constant 
main :: IO()
main = do 
    qs <- sample' (rExp difficulty)
    let q = last qs
    let a = show $ eval q
    putStrLn $ "What is the value of " ++ show q
    ans <- getLine
    putStrLn $ if (ans == a)
                  then "Correct"
                  else "Fail! Correct answer was: " ++ show a
    main      
          
          
          
          
          
          
          
          
          


