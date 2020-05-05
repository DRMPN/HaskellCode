module Queue where

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
