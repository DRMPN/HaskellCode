module Queue
( Queue,
  emptyQ,   -- Queue a
  isEmptyQ, -- Queue a -> Bool
  addQ,     -- a -> Queue a -> Queue a
  remQ      -- Queue a -> (a, Queue a)
  ) where

newtype Queue a = Qu [a]

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

{-
data Queue a = Qu [a] [a]

emptyQ = Qu [] []

isEmptyQ (Qu [] []) = True
isEmptyQ _ = False

addQ x (Qu xs ys) = Qu xs (x:ys)

remQ (Qu (x:xs) ys) = (x, Qu xs ys)
remQ (Qu [] (y:ys)) = remQ (Qu (reverse (y:ys)) [])
remQ (Qu [] []) = error "remQ"
-}

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
