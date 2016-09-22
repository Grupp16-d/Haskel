-- Föreläsning 2016 09 16
-- skriv text i en fil
-- wirteFile "myfile.txt" "Anna + Kalle\n = sant"
-- :t writeFile :: FilePath -> String -> IO ()
--              just a string        INSTRUCTIONS
 
-- :i readFile
-- readFile :: FilePath -> IO String
-- on screen is not the result of the function but the result of running the INSTRUCTION

-- Readfile
-- putStrLn -- write out a string with multiple lines when it has the "\n" 

-- skriver ut vad writeFile gör 
verboseWritefile :: FilePath -> String -> IO()
verboseWritefile file contents =
    do putStrLn ("Writing the file" ++ file)
       putStrLn ("The contents are " ++ take 20 contents ++ "...")
       writeFile file contents
       putStrLn "Done!"
       
-- copy a file 
copyFile :: FilePath -> FilePath -> IO()
copyFile file1 file2 =
    do s <- readFile file1
       writeFile file2 s
       
-- returns the length of a file
lengthFile :: FilePath -> IO Int
lengthFile file = do
    s <- readFile file
    return (length s)