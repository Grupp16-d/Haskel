-- Intro till funktionell programering 2/9 - 2016
-- se block för tidigare del
import Test.QuickCheck

data Suit = Spades | Clubs | Hearts | Diamonds
    deriving (Eq, Show)
-- delarna kan ha egna (numeric byggs upp mha ints)
data Rank = Numeric Int | Jack | Queen | King | Ace
	deriving (Eq, Show)
-- numeric är en dunktion Int -> Rank

legal :: Rank -> Bool
legal (Numeric n) = n > 1 && n < 11
legal _ 		  = True 
-- Hoogle lätt att söka på funktioner Sök mha typer dvs tex Bool -> Bool -> Bool

rankBeats :: Rank -> Rank -> Bool
-- inget slår ett ess
rankBeats _     Ace   = False
rankBeats Ace   _     = True
rankBeats _ 	King  = False
rankBeats King  _     = True
rankBeats _ 	Queen = False
rankBeats Queen _	  = True
rankBeats _ 	Jack  = False
rankBeats Jack  _	  = True
rankBeats (Numeric m) (Numeric n) = m > n

prop_rankBeats r1 r2 = rankBeats r1 r2 || rankBeats r2 r1 || r1 == r2

data Card = Card Rank Suit
    deriving (Eq,Show)
    
-- Example
acespades :: Card
acespades = Card Ace Spades

aceking :: Card
aceking = Card King Spades
-- Exemple Slut

rank :: Card -> Rank
suit :: Card -> Suit

rank (Card r _) = r
suit (Card _ s) = s

-- data Card = Card {rank::Rank, suit::Suit} 
-- [Gör samma sak som line 31,32, 39 - 43]

--With pattern matching
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1 == s2 && rankBeats r1 r2

--alternativt sett att skriva line 51, 52
--cardBeats c1 c2 = suit c1 == suit c2 && rankBeats (rank c1) (rank c2) 

--Rekursive data type
data Hand = Empty | Add Card Hand
    deriving Show

egHand1 :: Hand    
egHand1 = Add acespades Empty
egHand2 = Add (Card King Diamonds) egHand1

-- Size of a hand of cards
size :: Hand -> Int
size Empty     = 0
size (Add _ h) = 1 + size h

jacks :: Hand -> Int 
jacks h = countRank Jack h
kings h = countRank King h
          
--DONT DO COPY PASTE PROGRAMING 
countRank :: Rank -> Hand -> Int
countRank r Empty = 0
countRank r (Add c h) | rank c == r = 1 + countRank r h 
                      | otherwise   = countRank r h

-- handBeats: does one hand beat a given card
handBeats :: Hand -> Card -> Bool
handBeats Empty      c = False
handBeats (Add c' h) c = cardBeats c' c || handBeats h c
 
-- Föreläsning 09/09 2016

size :: Hand -> Int
size Empty = 0
size (Add c h) = 1 + size h

data ListInt    = EmptyInt | AddInt Int ListInt
data ListString = EmptyString | AddString String ListString