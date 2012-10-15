---
title: Purely Functional Data Structures: Random-access List
description: Implementation of a random-access list with O(1) list primitives and O(lg n) lookup time.
tags: algorithms, haskell, programming praxis
...

[Yesterday's Programming Praxis
task](http://programmingpraxis.com/2012/08/28/random-access-lists/) was to
implement Chris Okasaki's purely functional random-access list as described in
his 1995 paper[^okasaki1995].

Okasaki's random-access list preserves the $O(1)$ time guarantee of standard
lists of the primitive list operations `head`, `cons`, `tail` while adding the
possibility to access or update elements at a given index in $O(\lg n)$ time.
(These operations are $O(i)$ on standard lists, where $i$ is the index.)

This is done by maintaining a list of complete binary trees -- i.e. trees where
all nodes are leaves or have exactly two children. The nodes are stored in
*preorder* so that the first node of a tree is the head of the list that tree
represents.

It can be shown that we only need a logarithmic number of trees by noting that
each complete binary tree has height $2^{k}-1$ for $k>1$ and that any integer
$n\leq0$ can be written as a sum of $2^k-1$ terms. This decomposition is unique
if we require the terms to be as large as possible. This *greedy* decomposition
has at most $\lceil\lg(n+1)\rceil$ terms.

We thus have $O(\lg n)$ time to find the right tree in the list of trees and
$O(\lg n)$ time to find the requested node in the tree.

Okasaki goes on tho show that the worst-case time for lookups/updates is
actually $O(min\{i,\lg n\})$ -- meaning that these random-access lists are never
less efficient -- and further that the expected time is $O(\lg i)$.

The following Haskell implementation mirrors closely the Stanard ML
implementation in Okasaki's paper.

```haskell
import Prelude hiding (head, tail)
import Data.List (mapAccumL)

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show, Eq)

type RAList a = [(Int,Tree a)]

-- constructs an empty RAList
empty :: RAList a
empty = []

-- constructs an RAList from a list (O(n))
fromList :: [a] -> RAList a
fromList l = fst $ mapAccumL (\l e -> (cons e l,l)) empty (reverse l)

-- converts an RAList to a list (O(n))
toList :: RAList a -> [a]
toList [] = []
toList xs = head xs:toList (tail xs)


-- takes an RAList and an index and returns the item at that index (O(lg n))
index :: RAList a -> Int -> a
index ((size,t):r) i = if i < size
                               then index' (size,t) i
                               else index r (i-size)
  where index' (size, Leaf x) 0 = x
        index' (size, Leaf x) i = error "index: index out of bounds"
        index' (size, Node x t1 t2) 0 = x
        index' (size, Node x t1 t2) i =
              let size' = size `div` 2
                in if i <= size'
                      then index' (size',t1) (i-1)
                      else index' (size',t2) (i-1-size')

-- takes an RAList, an index and a value and updates the value at the given
-- position to the given value (O(lg n))
update :: RAList a -> Int -> a -> RAList a
update ((size,t):r) i e = if i < size
                               then (size,update' size t i e):r
                               else update r (i-size) e
  where update' size (Leaf x)       0 y = Leaf y
        update' size (Leaf _)       i y = error "update: index out of bounds"
        update' size (Node x t1 t2) 0 y = Node y t1 t2
        update' size (Node x t1 t2) i y =
              let size' = size `div` 2
               in if i <= size'
                     then Node x (update' size' t1 (i-1) y) t2
                     else Node x t1 (update' size' t2 (i-1-size') y)

-- prepends the given value to the given list (O(1))
cons :: a -> RAList a -> RAList a
cons x xs@((s1,t1):(s2,t2):rest) =
      if s1 == s2
         then (1+s1+s2,Node x t1 t2):rest
         else (1,Leaf x):xs
cons x xs = (1,Leaf x):xs

tail :: RAList a -> RAList a
tail ((_,Leaf _):rest) = rest
tail ((s,Node _ t1 t2):rest) = (s',t1):(s',t2):rest
  where s' = (s-1) `div` 2

-- returns the list's head (O(1))
head :: RAList a -> a
head []                 = error "head: empty list"
head ((_,Leaf x):_)     = x
head ((_,Node x _ _):_) = x

main = do
      -- some simple tests
      let t = fromList [3,2,1]
      print $ map (t `index`) [0,1,2] == [3,2,1]
      print $ head t == 3
      print $ toList (tail t) == [2,1]

      let t' = update t 2 0
      print $ map (t' `index`) [0,1,2] == [3,2,0]
```

[^okasaki1995]: [Chris Okasaki: *Purely Functional Random-Access Lists*](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.55.5156). In *Functional Programming Languages and Computer Architecture*, pages 86--95, 1995.

