import System.Random
import Data.List

-- Hangman
-- Here are there supposed to be code for hangman

-- returns the length of a file
{-lengthFile :: FilePath -> IO Int
lengthFile file = do
    s <- readFile file
    return (length s) -}
    
-- Non IO part
showWord :: String -> String -> String
showWord w gs = [reveal c | c <- w]
    where reveal c | c `elem` gs = c
                   | otherwise = '_'

-- How many time I can guess before i loose 
guesslimit = 6
                   
-- How many lives do you have                    
lives :: String -> String -> Int
lives w gs = guesslimit - length badGuesses
    where badGuesses = [c | c <- gs, c `notElem` w]

won,lost :: String -> String -> Bool 
won w gs  = '_' `notElem` showWord w gs
lost w gs = lives w gs == 0

-- IO part 
main :: IO()
main = do w <- randomWord
          play w []


-- dict = en file path för en list med ord
dict = "myfile.txt"
-- dictLength = längden av filen
dictLength = length dict
-- randomRIO ger ett random nummer
-- !! plocker det n:te elementet i en lista 

randomWord :: IO String
randomWord = do 
    s <- readFile dict 
    i <- randomRIO(0,dictLength-1)
    return (words s !! i)

-- Have 3 cases
  -- Player have lost, won or is in the midle of the game
  -- Show function converts something to a string 
  -- function getLine gets typed line from the terminal
play :: String -> String -> IO()
play w gs | won w gs  = putStrLn "Yay you Won!!"
          | lost w gs = putStrLn ("You lost. The word was: " ++ w)
          | otherwise = do 
                      putStrLn ("Lives: " ++ show(lives w gs))
                      putStrLn (showWord w gs )
                      putStr "Type your next guess: "
                      s <- getLine
                      play w (take 1 s ++ gs)
--                      



















