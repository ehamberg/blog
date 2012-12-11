---
title: Solitaire Cipher
description: Implementation of the Solitaire cipher from "Cryptonomicon"
tags: crypto, haskell, programming
---

I finally read [Neal Stephenson's book
*Cryptonomicon*](http://http://en.wikipedia.org/wiki/Cryptonomicon) this summer
which -- besides being a great read -- introduced an interesting cipher called
*Pontifex*. This cipher is based on the [Solitaire
cipher](http://www.schneier.com/solitaire.html) by Bruce Schneier and the idea
is that one simply needs a deck of card in order to communicate securely if a
list of keys has been exchanged. When seeing that yesterday's challenge on
Programming Praxis was to implement this cipher I simply *had* to do it, and
here's the resulting implementation, in Haskell, of the Solitaire cipher:

```haskell
import Data.Char (ord, chr)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Arrow

data Card a = Card a | JokerA | JokerB deriving (Show, Eq)
type Deck = [Card Int]

-- we start with a deck in bridge order
startDeck :: Deck
startDeck = map Card [1..52] ++ [JokerA] ++ [JokerB]

-- ‘A’ represents 1, ‘B’ 2, … , ‘Z‘ represents 26
charNum :: Char -> Int
charNum c = 1 + ord c - ord 'A'

-- 1 = ‘A’, … , 26 = ‘Z’, 27 = ‘A’, …
numChar :: Int -> Char
numChar n
  | n > 26    = numChar (n-26)
  | otherwise = chr (n+ord 'A'-1)

-- insert an element at a given position in a list
insertAt :: Int -> a -> [a] -> [a]
insertAt i e l = take i' l ++ [e] ++ drop i' l
    where i' = if i <= length l
                  then i
                  else i `mod` length l

-- regular cards have a value, 1 ≤ v ≤ 52, jokers have the value 53
cardVal :: Card Int -> Int
cardVal (Card n) = n
cardVal _        = 53

-- a counted cut takes n cards from the top of the deck and places them just
-- over the bottommost card
countedCut :: Deck -> Int -> Deck
countedCut d n = (init . drop n) d ++ take n d ++ [last d]

-- operation 1: the “A” joker is moved one card down the deck
op1 :: Deck -> Deck
op1 d = insertAt (i+1) JokerA (filter (/= JokerA) d)
    where i = fromJust $ elemIndex JokerA d

-- operation 2: the “B” joker is moved two cards down the deck
op2 :: Deck -> Deck
op2 d = insertAt (i+2) JokerB (filter (/= JokerB) d)
    where i = fromJust $ elemIndex JokerB d


-- operation 3: a triple-cut swaps all the cards above the highest joker in the
-- deck with all the cards below the lowest joker in the deck, leaving the two
-- jokers and the cards between them in place
op3 :: Deck -> Deck
op3 d = d3 ++ [d!!min j1 j2] ++ d2 ++ [d!!max j1 j2] ++ d1
    where joker c = c `elem` [JokerA, JokerB]
          j1      = fromJust $ elemIndex JokerA d
          j2      = fromJust $ elemIndex JokerB d
          d1      = take (min j1 j2) d
          d2      = takeWhile (not . joker) $ drop (min j1 j2 + 1) d
          d3      = drop (max j1 j2 + 1) d

-- operation 4: a counted cut, based on the number of the bottom card in the
-- deck, moves the top “count” cards to just above the bottom card
op4 :: Deck -> Deck
op4 d = countedCut d ((last >>> cardVal) d)

-- one step of the algorithm is the four operations above in sequence
step :: Deck -> Deck
step = op1 >>> op2 >>> op3 >>> op4

-- keying a deck consists of one step, and then, for each character in the key,
-- do a counted cut on the number of the current character followed by another
-- single step
keyDeck :: String -> Deck
keyDeck = foldl (\x c -> step (countedCut x (charNum c))) (step startDeck)

-- a keyed deck is key stream, each card representing a number 1 ≤ n ≤ 52
keyStream :: Deck -> [Int]
keyStream d@(c:cs) = [val | val /= 53] ++ keyStream (step d)
    where val = cardVal (d!!cardVal c)

-- encryption adds each character value to the value of the corresponding key
encrypt :: String -> String -> String
encrypt key plaintext = five $ zipWith (\a b -> cAdd a (numChar b)) text keys
    where deck               = keyDeck key
          keys               = keyStream deck
          text               = filter (/= ' ') plaintext
          cAdd a b           = numChar (charNum a + charNum b)
          five (a:b:c:d:e:t) = a:b:c:d:e:' ':five t
          five s             = s



-- decryption subtracts each character value from the value of the
-- corresponding key
decrypt :: String -> String -> String
decrypt key plaintext = zipWith (\a b -> cSub a (numChar b)) text keys
    where deck     = keyDeck key
          keys     = keyStream deck
          text     = filter (/= ' ') plaintext
          cSub a b = numChar (charNum a - charNum b)

main = do
    putStrLn $ encrypt "" "AAAAAAAAAA"
    putStrLn $ encrypt "FOO" "AAAAAAAAAAAAAAA"
    putStrLn $ encrypt "CRYPTONOMICON" "SOLITAIRE"
```
