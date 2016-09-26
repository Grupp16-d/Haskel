module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.Maybe

example :: Sudoku
example =
  Sudoku
    [ [Just 9, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]
-------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
 deriving ( Show, Eq )

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | x <- [1..9]] | y <- [1..9]]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = and [numRows sud, numElem sud, valueElem sud]
  where
    numRows   sud = length (rows sud) == 9
    numElem   sud = and [length r == 9| r <- rows sud]
    valueElem sud = and [and [x == Nothing || (x > Just 0 && x < Just 10)
                                               | x <- r] | r <- rows sud]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and [Nothing `notElem` r | r <- rows sud]

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr $ unlines [[printElem e | e <- r] | r <- rows sud]
  where
    printElem e | e == Nothing = '.'
                | otherwise    = intToDigit(fromJust e)

-- readSudoku file reads from the file, and either delivers it,
-- or stops if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
    s <- readFile file
    if (isSudoku(symCon s)) then return (symCon s)
            else error ("Exception: Not a Sudoku!")
 where -- symCon takes the string of  '.' and numbers and returns a suduko
    symCon s = Sudoku [[symCon' e | e <- r] | r <- lines s]
         where
            symCon' e | e == '.'  = Nothing
                      | otherwise = Just (digitToInt e)

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (1, do n <- choose (1,9)
                                              return (Just n))]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- test that generated sudukos are ture
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
-------------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve s | undefined = Nothing  -- There's a violation in s
        | undefined = Just s   -- s is already solved
        | otherwise = pickASolution possibleSolutions
  where
    nineUpdatedSuds   = undefined :: [Sudoku]
    possibleSolutions = [solve s' | s' <- nineUpdatedSuds]

pickASolution :: [Maybe Sudoku] -> Maybe Sudoku
pickASolution suds = undefined
