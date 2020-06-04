module Queue
( Queue,
  emptyQ,   -- Queue a
  isEmptyQ, -- Queue a -> Bool
  addQ,     -- a -> Queue a -> Queue a
  remQ      -- Queue a -> (a, Queue a)
  ) where

import Deque
import Data.List (elem)

newtype Queue a = Qu [a] deriving Show

emptyQ = Qu []

isEmptyQ (Qu []) = True
isEmptyQ _ = False

addQ x (Qu xs) = Qu (xs++[x])

remQ q@(Qu xs)
  | not (isEmptyQ q) = (head xs, Qu (tail xs))
  | otherwise = error "remQ"

-- add elements at the beginning
{-
addQ x (Qu xs) = Qu (x:xs)

remQ q@(Qu xs)
  | not (isEmptyQ q) = (last xs, Qu (init xs))
  | otherwise = error "remQ"
-}

data QueueN a = QuN [a] [a] deriving Show

emptyQN = QuN [] []

isEmptyQN (QuN [] []) = True
isEmptyQN _ = False

addQN x (QuN xs ys) = QuN xs (x:ys)

remQN (QuN (x:xs) ys) = (x, QuN xs ys)
remQN (QuN [] (y:ys)) = remQN (QuN (reverse (y:ys)) [])
remQN (QuN [] []) = error "remQ"

-- Ex 16.7
-- Give calcutation of
{-
"abcde" ++ "f"

[] ++ ys - False
(x:xs) ys - True
  a : "bcde" ++ "f"
  ...
    a : b : c : d : e : ([] "f" - True)
    "f"
  [] ++ ys - True
  a : b : c : d : e : "f"
  ...
"abcdef"

init "abcdef"
init x = take (length - 1) x

take (length "abcdef" - 1) "abcdef"
      length "abcdef" = 1 + length "bcdef" + ... = 6
      6 - 1 = 5
take 5 "abcdef"
a : take 4 "bcdef"
...
a : b : c : d : take 0 "ef"
a : b : c : d : []
"abcd"

last "abcdef"
last x = x !! (length x-1)

"abcdef" !! (length "abcdef" - 1)
"abcdef" !! (6 - 1)
"abcdef" !! 5
(!!) "abcdef" 5
     a : "bcdef" 4
     ...
     a : b : c : d : e : "f" 0
     'f'
-}

-- Ex 16.8
-- Explain the behavior of the three queue models
-- on the following sequence of operations:
-- add 2, add 1, remove, add 3, remove, add 1, add 4, remove, remove

  -- so behavior of 1qm and 2qm is the same
-- it adds an element to the end of the list
-- removes first element of the list

-- behavior of the third model
-- it will always add an element to the beginning of the second list
-- its removing first element of the first list
-- if the first list is empty, then it transfers reversed second list to the first list and removes its first element

-- Ex 16.9
-- imported from Deque.hs

-- Ex 16.10
-- A unique queue can contain only one occurrence of each entry
addUnQ x (Qu xs) =
  if elem x xs then Qu xs else Qu (xs ++ [x])
