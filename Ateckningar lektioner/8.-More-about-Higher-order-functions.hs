-- Föreläsning 2016 09 27

import Data.List

-- Group takes all elements that are equal and makes them to an own list
group' :: Eq a => [a] -> [[a]]
group' []     = []
group' (x:xs) = (x:takeWhile (== x) xs) : group' (dropWhile (== x) xs)

-- GroupBy the same as group just with your own equality notion
-- :t groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
{-
groupBy' :: Eq a => [a] -> [[a]]
groupBy' eq []     = []
groupBy' eq (x:xs) = (x:takeWhile (eq x) xs) : groupBy' (dropWhile (eq x) xs)
-}

-- Example Desing a function
--Input a string 
--Otput all words longer than 3 togherwith 
--the frequency of the word (most frequet first)



table = sortBy (\w v -> compare (snd v) (snd w )) 
	   . map (\ws -> (head ws, length ws)) 
	   . groupBy (==) 
       . sort
 	   . filter (\w -> length w > 3)
	   . words 