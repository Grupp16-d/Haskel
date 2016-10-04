-- 6. Föreläsning 2016-09-20
import Data.List(nub)
import Test.QuickCheck

 
getName = do 
    putStr "Type your name: "
    name <- getLine
    return name

--doTwice :: IO t -> IO (t,t)
-- doTwice :: Monad m => m t -> m(t,t)
-- Monad är en klass av olika typer instructioner
doTwice io = do
    a <- io
    b <- io
    return (a,b)

-- Information finns på PowerPoint 

-- arbitrary är en function som skapar värden till quickCheck
-- sample prints the value from a Gen 
-- om du inte vill att arbitrary 
-- ska producera () måste du ha t.ex. "(arbitrary :: Gen Int)"

-- producerar random naturliga nummer 
nats :: Gen Integer
nats = do 
    i <- arbitrary
    return (abs i)

-- generate a random even Integer
evens :: Gen Integer
evens = do 
    i <- arbitrary
    return (2 * i)
    
-- listOf skapar en lsita av Gen 
-- listOf1 skapar en lsita alla med minsta längd 1 
-- vectorOf skapar en lista med bestämd längd
-- choose (,) skapar en lista med elemet från ett viss intervall
-- elements väljer random element från en lista 

-- Examples From lab2: BlackJack

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Eq, Show)

rSuit :: Gen Suit
rSuit = elements[Spades, Hearts, Diamonds, Clubs]    
    
nonprop_Suit s1 s2 = s1 == (s2:: Suit)    
-- Makeing an instance
instance Arbitrary Suit where 
    arbitrary = rSuit 
    
data Rank = Numeric Integer | Jack | Queen | King | Ace
            deriving (Eq, Show)

rNumeric, rRoyal :: Gen Rank            
rNumeric = do
    i <- choose (2,10)
    return (Numeric i)
    
rRoyal = elements [Jack,Queen,King,Ace]

instance Arbitrary Rank where
    arbitrary = frequency [(4, rRoyal), (9, rNumeric)]
-- oneof plockar 1 Gen från en lista av Gen 
-- frequency plockar från lista med frekvensen av inten i paret
-- collect samlar data från quickcheck 
    
prop_rank' r = collect r (prop_rank r)

prop_rank (Numeric n) = n <= 10 && n > 1 
prop_rank _           = True
    
data Card = Card { rank :: Rank, suit :: Suit }
            deriving Eq

instance Show Card where
    show (Card r s) = show r ++ " of " ++ show s
    
instance Arbitrary Card where
    arbitrary = do
      r <- arbitrary
      s <- arbitrary
      return (Card r s)

data Hand = Empty | Add Card Hand
            deriving (Eq, Show)
            
instance Arbitrary Hand where
    arbitrary = do 
        cs <- arbitrary -- Gen [Card]
        return (toHand (nub cs))
      where toHand []     = Empty
            toHand (c:cs) = Add c (toHand cs)
    
    
    
    
    
    
    
    
    
    
    
    
    