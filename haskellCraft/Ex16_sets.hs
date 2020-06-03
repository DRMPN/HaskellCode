module Set
  ( Set,
    empty,              -- Set a
    sing,               -- a -> Set a
    memSet,             -- Ord a => Set a -> a -> Bool
    union, inter, diff, -- Ord a => Set a -> Set a -> Set a
    eqSet,              -- Eq a => Set a -> Set a -> Bool
    subSet,             -- Ord a => Set a -> Set a -> Bool
    makeSet,            -- Ord a => [a] -> Set a
    mapSet,             -- Ord b => (a -> b) -> Set a -> Set b
    filterSet,          -- (a -> Bool) -> Set a -> Set a
    foldSet,            -- (a -> a -> a) -> a -> Set a -> Set a
    showSet,            -- (a -> String) -> Set a -> String
    card                -- Set a -> Int
  ) where

import Data.List hiding ( union )

newtype Set s = SetI [s]

instance Eq a => Eq (Set a) where
  (==) = eqSet
instance Ord a => Ord (Set a) where
  (<=) = leqSet
instance Show a => Show (Set a) where
  show (SetI x) = "SetI " ++ show x

empty :: Set a
empty = SetI []

sing :: a -> Set a
sing x = SetI [x]

memSet :: Ord a => Set a -> a -> Bool
memSet (SetI []) y = False
memSet (SetI (x:xs)) y
  | x < y = memSet (SetI xs) y
  | x == y = True
  | otherwise = False

-- union {Joe,Sue} {Sue,Ben} = {Joe,Sue,Ben}
union :: Ord a => Set a -> Set a -> Set a
union (SetI xs) (SetI ys) = SetI (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni xn@(x:xs) yn@(y:ys)
  | x < y = x : uni xs yn
  | x == y = x : uni xs ys
  | otherwise = y : uni xn ys

-- inter {Joe,Sue} {Sue,Ben} = {Sue}
inter :: Ord a => Set a -> Set a -> Set a
inter (SetI xs) (SetI ys) = SetI (int xs ys)

int :: Ord a => [a] -> [a] -> [a]
int [] ys = []
int xs [] = []
int xn@(x:xs) yn@(y:ys)
  | x < y = int xs yn
  | x == y = x : int xs ys
  | otherwise = int xn ys

-- Ex 16.33
-- Define the function diff so that diff s1 s2
-- consists of the elements of s1 which do not belings to s2
-- diff {Joe,Sue} {Sue,Ben} = {Joe}
diff :: Ord a => Set a -> Set a -> Set a
diff (SetI xs) (SetI ys) = SetI (dif xs ys)

dif :: Ord a => [a] -> [a] -> [a]
dif [] ys = []
dif xs [] = xs
dif xn@(x:xs) yn@(y:ys)
  | x < y = x : dif xs yn
  | x == y = dif xs ys
  | otherwise = dif xn ys

subSet :: Ord a => Set a -> Set a -> Bool
subSet (SetI xs) (SetI ys) = subS xs ys

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys = True
subS xs [] = False
subS (x:xs) (y:ys)
  | x < y = False
  | x == y = subS xs ys
  | otherwise = subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (SetI xs) (SetI ys) = xs == ys

leqSet :: (Eq a, Ord a) => Set a -> Set a -> Bool
leqSet (SetI xs) (SetI ys) = xs <= ys

makeSet :: Ord a => [a] -> Set a
makeSet = SetI . remDups . sort
  where
    remDups [] = []
    remDups [x] = [x]
    remDups (x:y:xs)
      | x < y = x : remDups (y:xs)
      | otherwise = remDups (y:xs)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (SetI xs) = SetI (map f xs)

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (SetI xs) = SetI (filter p xs)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f x (SetI xs) = foldr f x xs

showSet :: (a -> String) -> Set a -> String
showSet f (SetI xs) = concat (map ((++"\n") . f) xs)

-- cardinality
card :: Set a -> Int
card (SetI xs) = length xs

-- Ex 16.32
-- Compare how the following pairs of sets are related
-- by the orderings <= and subSet
{-
{3} {3,4}
  <=     -> True
  subSet -> True
{2,3} {3,4}
  <=     -> True
  subSet -> False
{2,9} {2,7,9}
  <=     -> False
  subSet -> True
-}

-- Ex 16.34
-- Define function which gives the symmetric difference of two sets.
-- This consist of the elements which lie in one of the sets
-- but not the other, so that
-- symmDiff {Joe,Sue} {Sue,Ben} = {Joe,Ben}
symmDiff :: Ord a => Set a -> Set a -> Set a
symmDiff x y = SetI (diffl ++ diffr)
  where
    diffl = unSet $ diff x y
    diffr = unSet $ diff y x
    unSet (SetI x) = x
-- or I can implement function joinSet that combines two sets together

-- Ex 16.35
-- Define the function which returns the set of all subsets of a set defined
powerSet :: Ord a => Set a -> Set (Set a)
powerSet (SetI xs) = SetI (map makeSet $ pwrst xs)
pwrst :: Ord a => [a] -> [[a]]
pwrst [] = [ [] ]
pwrst (x:xs) = map (x:) (pwrst xs) ++ pwrst xs

-- Recurtion definition of power set from wiki
psWiki [] = [[]]
psWiki (x:xs) = psWiki xs ++ [ x:t | t <- psWiki xs]

-- Ex 16.36
-- Define functions which return union and intersection of a set of sets
-- defined using the operations of the abstract data type.
setUnion :: Ord a => Set (Set a) -> Set a
setUnion (SetI xs) = foldr1 (\x acc -> union acc x) xs

setInter :: Ord a => Set (Set a) -> Set a
setInter (SetI xs) = foldr1 (\x acc -> inter acc x) xs
