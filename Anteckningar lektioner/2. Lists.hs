-- Lectures
import Test.QuickCheck
-- Creat our own list
data ListInt    = EmptyInt | AddInt Int ListInt
data ListString = EmptyString | AddString String ListString

-- Instead of haveing copy paste code we have a parameter in our data type
data List a = Empty | Add a (List a)
    deriving (Eq,Show)   
    
eg1 :: List Bool
eg1 = Add True Empty

eg2 = Add "Hello" (Add "World" Empty)

-- Make our own functions over lists

-- Lenght 
size :: [a] -> Int -- A polymorthic function, a function with a type variable 
size []     = 0
size (x:xs) = 1 + size xs

-- Sum
-- Num funtions says that the typ must be a number eg Int or Dubble
sum' :: Num t => [t] -> t
sum' []     = 0 
sum' (n:ns) = n + sum' ns

-- maximum
-- can be used fpr other thins than num-class types
-- if used on stings it takes the sum of unicode numbers eg there are 
-- a differens betewen "A" "a"
maximum' :: Ord t => [t] -> t
maximum' [] = error "Empty List"
-- [x] = (x:[]) therefor it can be used in patternmatching
maximum' [x] = x -- x is the only element therefor its allways the biggest
maximum' (x:xs) = max x (maximum' xs)

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

head' (x:xs) = x

last' [x]    = x
last' (_:xs) = last' xs

last'' xs = head' (reverse xs)

prop_initlast :: [Bool] -> Property
prop_initlast xs = xs /= [] ==> 
              init xs ++ [last xs] == xs

-- Lecture 2016-09-13
-- line 40 - 42 is not a popular way to define it because the append (++) function
-- append takes about the same number of steps as the length of the list you append
-- smarter way to reverse a list

rev:: [a] -> [a]
rev xs = revHelper xs []
-- acc stand for acculating, meaning a place where you start building something
    where 
      revHelper []     acc = acc
      revHelper (x:xs) acc = revHelper xs (x:acc)
{- 
rev step-by-step
rev [1,2]
== rev (1:2:[])
== revHelper (1:2:[]) []
== revHelper (2:[])   (1:[])
== revHelper ([])     (2:1:[])
== (2:1:[])
== [2,1]
-}

-- take, drop

take', drop' :: Int -> [a] -> [a]
take' n _  | n <= 0  = []
take' _ []           = []
take' n (x:xs)       = x:take'(n-1) xs

drop' n xs | n <= 0  = xs
drop' _ []           = []
drop' n (x:xs)       = drop' (n-1) xs

--använder att take' och drop' är lika för att testa om de fungerar
-- verboseCheck skriver ut testen i terminalen 
-- quickCheck testar med () => kommer alltd stämma 
prop_takedrop n xs = take' n xs ++ drop' n xs == xs
    where types = xs :: [Bool] -- tvingar quickCheck att testa med en lista av bool







