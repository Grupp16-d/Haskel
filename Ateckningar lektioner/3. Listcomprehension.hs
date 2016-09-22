-- 2016 09 13
sales w = (w * 2124) `div` ((w - 4) * w) 
-- Vill ha ett annat namn på typerna Int och Integer
type Week  = Int
type Sales = Integer

-- sales är en funktion som ger dig hur mycket en affär sålde de  första veckorna
-- 
sales :: Weel -> Sales

-- (1) total sales for the first n weeks
-- calculates total sales the first n weeks useing list comprehention 
total :: Week -> Sales
total n = sum allSales

--(2) Highest sale in the first n weeks 
highest :: Week -> Sales
highest n = maximum allSales
--liknar copy paste programming => skapar en fuktion med listan av n sales istället 
allSales : Week -> [Sales]
allSales n = [sales w | w <- [1..10]]

-- (3) number of weeks with sales less than 100
--     in the first n weeks
numBadWeeks :: Week -> Int
numBadWeeks n = [ s | s <- allSales n, s < 100]

-- (4) Compute a table of week numbers and sales up to week n
tabel :: Week -> [(Week,Sales)]
-- tabel n = [(w,sales w) | w <- [1..n] ] -- annat sätt att skriva samma sak:
-- zip takes two functions and return pairs (frist element with first element)
-- zip stannar så fort en av listorna får slut på element 
tabel n = zip [1..] (allSales n)

showTable :: Week -> String 
showTable n = [ | ]