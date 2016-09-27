import Data.Char
-- Föreläsning 23 09 2016

--why we need HOF:s (higher order functions)

sum' [] = 0
sum' (n:ns) = n + sum' ns

-- the empty list = 1 because 1 is the identety for multiplication 
product' []    = 1
product' (n:ns) = n * product' ns

-- concat takes list of list and makes them to one list
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- all the functions look verry alike ("copy-past programing") 
-- we fix this by makeing a new function foldr

-- b = basecase, f = the operator or the funtion
foldr' f b []     = b
foldr' f b (x:xs) = x `f` foldr' f b xs

{-psedo code 
foldr op base (a:b:c:...) == a `op` (b `op` (... `op` base)...) 
"think about foldr as changeing all the : with the operator" 

foldl "fold left" start building from the left
foldl op base (a:b:c:...) == ((base `op` a) `op` b) ...) 
-}

-- unlines takes a list of strings and make one big string with \n at every new list
verse :: String
verse = unlines 
    [ "Livet är härligt"
    , "Tavartj vårt liv är härligt"
    , "Vi alla våra små beskymer glömmer"
    , "..."
    ]
    
-- takeLine, takes the fist row of a multiline string 
takeLine [] = []
takeLine (c:cs) 
    | c /= '\n' = c:takeLine cs
    | otherwise = []



     
 
-- isSpace checks if a caracter is a unicode space incuding \n ...
takeWord [] = []
takeWord (c:cs)
    | not(isSpace c) = c:takeWord cs 
    | otherwise      = []
    
takeWord' cs = takeWhile notSpace cs 
     where notSpace c = not(isSpace c)

-- a way of defineing a function without giveing it a name
    -- \c -> not(isSpace c)    
takeWord'' cs = takeWhile (\c -> not(isSpace c)) cs     

-- p = predicate, meaning a function 
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x:takeWhile' p xs
                    | otherwise = []
    
-- you can make an funtion by leaving out a argument in a operator 
-- e.g map (1+) [1..3] == [2..4]

-- lines takes a string and breaks it up in smal strings at every \n
-- dropWhile throws away all element untill it 
lines' [] = []
lines' xs = takeWhile (/= '\n') xs : lines' (drop 1(dropWhile (/= '\n') xs))

segments p [] = []
segments p xs = takeWhile p xs : lines' (drop 1(dropWhile p xs))

-- '.' is like the smal cirkle in math (combind two functions)
-- f . g = \x -> f ( g x)

words' cs = segments (not.isSpace) cs

rep :: Int -> String -> Char -> String
rep n s c = concat (replicate n (s++[c]))

repfyr :: String -> Char -> String
repfyr = rep 4

fyrfallig :: Char -> String
fyrfallig = repfyr "hurra"

-- hlint <filename> to find this kind od thing 













