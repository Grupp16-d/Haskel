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
example2 :: Sudoku
example2 =
   Sudoku
    [ [Just 1, Just 1, Just 1, Just 1, Just 1, Just 1, Just 1, Just 1, Just 1]
    , [Just 2, Just 2, Just 2, Just 2, Just 2, Just 2, Just 2, Just 2, Just 2]
    , [Just 3, Just 3, Just 3, Just 3, Just 3, Just 3, Just 3, Just 3, Just 3]
    , [Just 4, Just 4, Just 4, Just 4, Just 4, Just 4, Just 4, Just 4, Just 4]
    , [Just 5, Just 5, Just 5, Just 5, Just 5, Just 5, Just 5, Just 5, Just 5]
    , [Just 6, Just 6, Just 6, Just 6, Just 6, Just 6, Just 6, Just 6 ,Just 6]
    , [Just 7, Just 7, Just 7, Just 7, Just 7, Just 7, Just 7, Just 7 ,Just 7]
    , [Just 8, Just 8, Just 8, Just 8, Just 8, Just 8, Just 8, Just 8 ,Just 8]
    , [Just 9, Just 9, Just 9, Just 9, Just 9, Just 9, Just 9, Just 9 ,Just 9]
    ]

-------------------------------------------------------------------------
data Sudoku = Sudoku [[Maybe Int]]
 deriving ( Show, Eq )

type Block = [Maybe Int]
type Pos = (Int, Int)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- Assignment A
-------------------------------------------------------------------------
-- A1
-- AllBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | x <- [1..9]] | y <- [1..9]]

-- A2
-- IsSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = and [numRows sud, numElem sud, valueElem sud]
  where
    numRows   sud = length (rows sud) == 9
    numElem   sud = and [length r == 9| r <- rows sud]
    valueElem sud = and [and [x == Nothing || (x > Just 0 && x < Just 10)
                                               | x <- r] | r <- rows sud]
-- A3
-- IsSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = and [Nothing `notElem` r | r <- rows sud]

-- Assignment B
-------------------------------------------------------------------------
-- B1
-- PrintSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStr $ unlines [[printElem e | e <- r] | r <- rows sud]
  where
    printElem e | e == Nothing = '.'
                | otherwise    = intToDigit(fromJust e)

-- B2
-- ReadSudoku file reads from the file, and either delivers it,
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

-- Assignment C
-------------------------------------------------------------------------
-- C1
-- Cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (1, do n <- choose (1,9)
                                              return (Just n))]
-- C2
-- An instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- C3
-- Test that generated sudukos are ture
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud

-- Assignment D
-------------------------------------------------------------------------
-- D1
-- Remove all dublicate numbers, then checks that the lenght still is 9
isOkayBlock :: Block -> Bool
isOkayBlock b = (length $ nubBy (\x y -> x == y && (x /= Nothing)) b) == 9

{-FUNGERAR INTE MÅSTE LÄGGA TILL BLOCKS
Måste oxå lägga till property-}
-- D2
-- Breaks up a Sudoku in blocks (9 rows, 9 colums and 9 3*3 blocks)
blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ transpose (rows sud)

test []         = []
test (a:b:c:ds) = [a,b,c] : test ds

test1 = [test r | r <- [[1..9],[11..19]]]
-- Property for blcoks funktion check if there are 3*9 blocks,
-- and each block has exactly 9 cells.
--prop_blocks =

-- D3
-- Check if the sudoku do not coantin dublicate numbers in a block
isOkay :: Sudoku -> Bool
isOkay sud = and [isOkayBlock b | b <- blocks sud]

-- Assignment E
-------------------------------------------------------------------------
-- E1
-- iRow = index for a row that contains a nothing
-- iPos = the position of the Nothing in the row
-- The function gives the position of the first Nothing
blank :: Sudoku -> Pos
blank sud = head [(iRow, iPos r)
   | (iRow, r) <- ([0..]:: [Int]) `zip` (rows sud), Nothing `elem` r]
  where iPos r = fromJust $ elemIndex Nothing r

-- Check that there are a Nothing at the possision given by blank
prop_Blank sud = (((rows sud) !! (fst $ blank sud))
                              !! (snd $ blank sud)) == Nothing

-- E2
-- Change the valeu of an element. wher the element is grater then 0 and
-- less then the leght of the list
(!!=) :: [a] -> (Int,a) -> [a]
list !!= (i, e)
    | i < 0 || i > (length list)-1 = list
    | otherwise                    = (take i list) ++ (e:(drop (i+1) list))

-- Check if the leght of the list is the same after change.
prop_ChangElem_size :: [Int] -> (Int,Int) -> Bool
prop_ChangElem_size list p = length list == (length $ list !!= p)


-- E3
--Change the valeu of a element in a givven possision
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (y,x) e = Sudoku (rows sud !!= (y, (rows sud !! y) !!= (x,e)))

-- Checks that the updated position really has gotten the new value
--prop_update :: [[Maybe Int]] -> (Int, Int) -> Maybe Int -> Bool
--prop_update sud (x, y) e = e == (rows update((rows sud) (x, y) (e))!! y) !! x

-- Assignment F
-------------------------------------------------------------------------
-- F1
--
solve :: Sudoku -> Maybe Sudoku
solve s | undefined = Nothing  -- There's a violation in s
        | undefined = Just s   -- s is already solved
        | otherwise = pickASolution possibleSolutions
  where
    nineUpdatedSuds   = undefined :: [Sudoku]
    possibleSolutions = [solve s' | s' <- nineUpdatedSuds]

pickASolution :: [Maybe Sudoku] -> Maybe Sudoku
pickASolution suds = undefined