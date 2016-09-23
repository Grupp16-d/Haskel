import MeasureTime
import Test.QuickCheck

-- The basic pwoer function
power :: Integer -> Integer -> Integer
-- Show an error message when k is negativ
power n k | k < 0 = 
    error "The funktion power do not support negative numbers"
-- Base value for this funktion based on math, n^0 = 1 
power n 0 = 1
power n k = n * power n (k-1)

{- Assingment 1 
It takes k+1 steps to calculate n to the power of k with the funktion power
-}

-- Assingment 2
power1::Integer -> Integer -> Integer
power1 n k | k < 0 = 
    error "The Funktion power1 do not support negastive numbers"
{-
It creats a list containing k number of n useing the built in function 
    replicate (eg. k = 2, n = 1 will give the list {1,1}) 
It then calculates the product of the list useing the built in function
    product (eg. when product is applide to the list {2,2,2} it will
    calculate 2*2*2)
-}
power1 n k = product (replicate (fromIntegral k) (fromIntegral n))

-- Assingment 3
power2 :: Integer -> Integer -> Integer
power2 n k  | k < 0 = 
    error "The Funktion power2 do not support negastive numbers"
power2 n 0 = 1
{- 
Checks if k is even then divivde k 2 to increas the calculating speed of 
    the funktion 
If k is not even frist use the the same formula as the original power
    function to make k even
-}
power2 n k 	| even k 	= power2 (n * n) (div k 2)
            | otherwise = n * power2 n (k-1)

-- Assingment 4s
{- part A)

Testcase 1: Small numbers
    Tests that the functions are calcutalting simple power-calcutaions
        correctly

Testcase 2: Negative base, even exponent
    When useing power on a negative base and an even exponent is should
        according to math return a posetive answer
 
Testcase 3: Negative base, uneven exponent
    When useing power on a negative base and an uneven exponent it sould
        according to math return a negative answer

Testcase 4: Exponent is 0
    When takeing somthing to the power of 0 according to math rules it
        should return 1 no matter what the base is
        -}
        
-- Part B)

-- Checks if given the same numbers power and power1 returns the same answer
comparePower1:: Integer -> Integer -> Bool 
comparePower1 n k = power n k == power1 n k

comparePower2:: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k

-- Part C)
{- Tests the funtions power, power1 och power2 and checks them against a
       given numbers -}
testFunctions:: Bool
testFunctions = and [comparePower1 x y && comparePower2 x y 
                        | x <- [(-10)..10], y <- [0..10]]



