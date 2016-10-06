import Data.Char(isSpace,isDigit)

data Expr 
    = Num Integer
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq, Show)

type Parser a = String -> Maybe (a,String)

number:: Parser Integer
number (c:cs) | isDigit c = Just (read digits, rest)
  where 
    digits = c:takeWhile isDigit cs
    rest   =   dropWhile isDigit cs

number _                  = Nothing

num :: Parser Expr
num s = case number s of
    Just(n,s1) -> Just(Num n,s1)
    _          -> Nothing

expr1 s = case num s of
  Just (ne ,'+':s1) -> case expr1 s1 of
                            Just (e,s2) -> Just (Add ne e, s2)
                            Nothing     -> Just (ne ,'+':s1)
  r                                     -> r


term = chain factor '*' mul
expr = chain term + '+'

chain p op f s = case p s of
      Just (ne ,c:s1) |c == op -> case chain p op f s1 of
                                Just (e,s2) -> Just (f ne e, s2)
                                Nothing     -> Just (ne ,c:s1)
  r                                         -> r





----------------------------------------------------------------
last3' :: [a] -> Maybe (a,a,a)
last3' xs = h (reverse xs) 
  where
    h (c:b:a:_) = Just (a,b,c)
    h _         = Nothing

last3 cs = case reverse xs of 
  (c:b:a:_) -> Just ( a,b,c)
  _         -> Nothing 
      