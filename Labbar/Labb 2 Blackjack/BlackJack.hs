module BlackJack where

import Cards
import Wrapper
import Test.QuickCheck hiding (shuffle)
import System.Random

-- Gives an alternative name for Hand to make the code more readable
type Deck = Hand
{- Task A

hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)

size hand2
    = size Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
    = 1 + size (Add (Card Jack Spades) Empty)
    = 1 + 1 + size Empty
    = 1 + 1 + 0
    = 2
-}

-- Task B

-- Examples used for testing
aCard1 :: Card
aCard1 = Card Ace Spades

aCard2 :: Card
aCard2 = Card (Numeric 5) Diamonds

aHand :: Hand 
aHand = Add (aCard1) (Add aCard2 Empty)

-- Calculates the value of a rank
valueRank :: Rank -> Integer 
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10 -- All other cards are either King, Queen or Jack

-- Calculates the value of a Card
valueCard :: Card -> Integer
valueCard (Card r1 s1) = valueRank r1

-- Calculates the number of aces in a given Hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                = 0
numberOfAces (Add (Card Ace _) h) = 1 + numberOfAces h
numberOfAces (Add _ h)            = 0 + numberOfAces h

-- Calculates the value of a hand
value :: Hand -> Integer
value h | (helpValue h) < 22 = helpValue h
        | otherwise          = (helpValue h) - ((numberOfAces h) * 10)
    where -- Calculates the value of the hand as if all aces were 11 
      helpValue :: Hand -> Integer
      helpValue Empty     = 0
      helpValue (Add c h) = (valueCard c) + helpValue h
      
-- Checks if a player have busted with a given hand n
gameOver :: Hand -> Bool
gameOver h = value h > 21 

-- Checks who winns given 2 hands (plyH = playerHand, bnkH = bankHand)
winner :: Hand -> Hand -> Player
winner plyH bnkH | gameOver plyH               = Bank
                 | gameOver bnkH               = Guest
                 | (value plyH) > (value bnkH) = Guest
                 | otherwise                   = Bank

-- Task C              
-- Given two hands it puts the first one (h1) ontop of the other (h2)
(<+) :: Hand -> Hand -> Hand
Empty       <+ h2 = h2
(Add c1 h1) <+ h2 = (Add c1 (h1 <+ h2)) 
   
-- Checks if the function (<+) is associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3
-- Checks if the size of the given hands is the same after merging 
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = (size p1 + size p2) == size (p1 <+ p2)

-- Task D                          
-- Returns a full deck of cards                  
fullDeck :: Deck
fullDeck = fullSuit ranks Hearts <+ 
           fullSuit ranks Spades <+ 
           fullSuit ranks Clubs  <+ 
           fullSuit ranks Diamonds
    where 
      ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]
      fullSuit :: [Rank] -> Suit -> Hand
      fullSuit [] _     = Empty
      fullSuit (r:rs) s = Add (Card r s) (fullSuit rs s)
          
-- Task E
-- Draws one card from the deck
draw :: Deck -> Hand -> (Deck, Hand)
draw Empty        _ = error "draw: The deck is empty."
draw (Add c deck) h = (deck, (Add c h))

-- Task F
-- the Bank draws cards until it has a hand vaule of 16
-- the bank plays its turn
playBank :: Deck -> Hand
playBank deck = snd(playBank' (deck, Empty))
   where      
     playBank' (deck, bHand) | value bHand < 16 =  playBank'(draw deck bHand)
                             | otherwise        = (deck, bHand)

-- Task G 
shuffle:: StdGen -> Deck -> Deck
shuffle g0 deck = shuffle' g0 (deck, Empty)
  where
    shuffle' :: StdGen -> (Deck,Deck) -> Deck
    shuffle' g0 (d,acc)
      | size d == 0 = acc -- whole deck is shuffled 
      | otherwise   = shuffle' g0 (pickNthCard (rngInt g0 d) (d,acc))
        -- rngInt returns a random Int when given an intervall and a StdGen
        where 
          rngInt g0 d = fst (randomR (1,size d) g0)      
    pickNthCard :: Int -> (Deck,Deck) -> (Deck,Hand)
    pickNthCard n (deck,acc) = pickNthCard' n (deck,acc) Empty
        where
          pickNthCard' :: Int -> (Deck,Deck) -> Hand -> (Deck,Hand) 
          pickNthCard' n ((Add c1 deck'),acc) acc1 
                | (size acc1) == (n-1) = ((deck' <+ acc1),(Add c1 acc))
                | otherwise            = pickNthCard' n (deck',acc) (Add c1 acc1)
-- properties for the shuffle funciton
-- tests that the same cards are in the deck before and after the shuffle
prop_shuffle_sameCards :: StdGen -> Card -> Deck -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == c `belongsTo` shuffle g h
    where 
      belongsTo :: Card -> Deck -> Bool
      c `belongsTo` Empty    = False
      c `belongsTo` Add c' h = c == c' || c `belongsTo` h
-- tests that the size stays the same after the deck have been shuffled
prop_size_shuffle :: StdGen -> Deck -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)


implementation = Interface
  {  iEmpty     = Empty
  ,  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }

main :: IO ()
main = runGame implementation





