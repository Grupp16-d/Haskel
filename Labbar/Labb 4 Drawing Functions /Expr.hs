module Expr where

import Test.QuickCheck
import Data.List
import Data.List.Split

-- | ------------------------- Part I --------------------------------|--
-- Assignment A
-------------------------------------------------------------------------
--
data Expr = Num Integer
        | add Expr Expr
        | mul Expr Expr
        
    deriving (Eq,Show)

-- Assignment B
-------------------------------------------------------------------------
--
showExpr :: Expr -> String
showExpr = undefined


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