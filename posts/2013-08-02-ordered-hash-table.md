---
title: Programming Praxis: Ordered Hash Tables
description: Chained Hash Table With Ordered Lists
tags: programming praxis, algorithms, haskell
...

[Today's Programming Praxis
problem](http://programmingpraxis.com/2013/08/02/ordered-hash-tables/) was to
create a hash table with linear probing, but where the buckets are ordered. This
is easily done by using the `insert`, `delete` and `find` functions from
`Data.List`.

This means that inserts take longer, but that lookups (even unsuccessful ones)
can – on average – terminate half-way through the list.

```haskell
import Prelude hiding (lookup)
import qualified Data.Vector as V
import Data.Hashable (Hashable, hash)
import qualified Data.List as L (delete, find, insert)
import Data.Maybe (isJust, isNothing)

data HT a = HT (V.Vector [a]) deriving Show

empty :: (Hashable a, Ord a) => Int -> HT a
empty size = HT (V.replicate size [])

insert :: (Hashable a, Ord a) => HT a -> a -> HT a
insert (HT bs) e = let pos    = hash e `mod` V.length bs
                       bucket = L.insert e $ bs V.! pos
                    in HT (V.update bs (V.singleton (pos,bucket)))

delete :: (Hashable a, Ord a) => HT a -> a -> HT a
delete (HT bs) e = let pos    = hash e `mod` V.length bs
                       bucket = L.delete e $ bs V.! pos
                    in HT (V.update bs (V.singleton (pos, bucket)))

lookup :: (Hashable a, Ord a) => HT a -> a -> Maybe a
lookup (HT bs) e = let pos    = hash e `mod` V.length bs
                       bucket = bs V.! pos
                    in L.find (==e) bucket

main :: IO ()
main = do
  -- some simple tests
  let ht   = foldr (flip insert) (empty 10 :: HT Int) [0..99]
  print . isNothing $ lookup ht 100
  print . isJust $ lookup ht 50
  print . isNothing $ lookup (delete ht 50) 50
  print . all isNothing $ map (lookup (foldr (flip delete) ht [0..99])) [0..99]
```
