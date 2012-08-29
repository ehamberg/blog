---
title: Programming Praxis: Hash Table With Open Addressing
description: Hask table with open addressing and deletion
tags: algorithms, haskell, programming praxis
...

Today's [Programming Praxis
task](http://programmingpraxis.com/2012/08/24/hash-tables-with-open-addressing/)
was to create a hash table using open addressing to resolve collisions and that
supports deletion.

The following, simple Haskell implementation of a hash table has a static size
and supports deletion by using the scheme suggested in the task, i.e. to mark
empty cells as `Nil` or `Deleted`. By doing this we can detect cells that
previously had an item there and thus continue searching in cases where a
deletion in the middle of a sequence of items with the same hash has occurred.

To avoid infinite loop when inserting into a full table or deleting for a table
filled with `Deleted` cells, both `insert` and `delete`'s helper functions are
passed an `end` parameter that indicates when all elements have been looped
over.

```haskell
{-# Language RankNTypes #-}

import Data.Hashable
import Data.Vector (Vector, (//), (!), fromList)
import qualified Data.Vector as V

type HashFunction = (Hashable a) => a -> Int

-- A hash table has a vector of cells, a size and a hash function
data HT a = HT
  { vector :: Vector (Item a)
  , size   :: Int
  , h      :: HashFunction
  }

instance (Show a) => Show (HT a) where
  show (HT v s _) = show (v,s)

-- A cell is nil, deleted or contains an item
data Item a = Nil | Deleted | Item a
  deriving (Show, Eq)

-- Creates a new hash table of the given size with the given hash function
createNewHT :: (Hashable a) => Int -> HashFunction -> HT a
createNewHT size h = HT (V.replicate size Nil) size h

-- Insert an item into the hash table. Searches from h(item) until a
-- Deleted/Nil cell is found and inserts the item there.
-- The “end” parameter to “doInsert” is set to the position before where we
-- start the search to avoid an infinite loop when the table is full.
insert :: (Hashable a) => HT a -> a -> HT a
insert (HT v s h) item = HT (doInsert v (h item) (h item `mod` s-1)) s h
  where doInsert v' h' end
          | end == h' = error "Insert on full table"
          | otherwise  = case v' ! (h' `mod` s) of
                              Item _ -> doInsert v' ((h'+1) `mod` s) end
                              _      -> v' // [(h' `mod` s,Item item)]

-- Delete an item. Linear scan from h(item), if Nil is found, we do nothing, if
-- Delete is found, we continue looking. If an Item is found and it is the same,
-- we delete it. The “end” parameter to “doDelete” is set to the position
-- before where we start the search to avoid an infinite loop when the table is
-- full of Deleted cells.
delete :: (Hashable a, Eq a) => HT a -> a -> HT a
delete (HT v s h) item = HT (doDelete v (h item) (h item `mod` s-1)) s h
  where doDelete v' h' end
          | end == h' = v' -- went through whole table without finding element
          | otherwise = case v' ! (h' `mod` s) of
                             Nil     -> v'
                             Deleted -> doDelete v' ((h'+1) `mod` s) end
                             Item i  -> if i == item
                                           then v' // [(h' `mod` s, Deleted)]
                                           else doDelete v' ((h'+1) `mod` s) end
                                           --
-- Some basic tests
main :: IO ()
main = do
  -- insert and delete
  let a = createNewHT 5 hash :: HT Int
  let a' = ((((a `insert` 3) `insert` 8) `insert` 13 ) `delete` 3) `delete` 13
  print $ vector a' == fromList [Deleted,Nil,Nil,Deleted,Item 8]

  -- delete item from empty table
  let b = createNewHT 2 hash :: HT Int
  let b' = ((((b `insert` 1) `insert` 2) `delete` 1) `delete` 2) `delete` 3
  print $ vector b' == fromList [Deleted,Deleted]
```
