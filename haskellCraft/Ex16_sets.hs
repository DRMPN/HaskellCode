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

import List hiding ( union )

newtype Set s = SetI [a]

instance Eq a => Eq (Set a) where
  (==) = eqSet
instance Ord a => Ord (Set a) where
  (<=) = leqSet

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
  | x = y = x : uni xs ys
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

-- define diff funciton
-- diff {Joe,Sue} {Sue,Ben} = {Joe}

subSet :: Ord a => Set a -> Set a -> Bool
subSet (SetI xs) (SetI ys) = subS xs ys

subS :: Ord a => [a] -> [a] -> Bool
subS [] ys = True
subS xs [] = False
subS (x:xs) (y:ys)
  | x < y = False
  | x = y = subS xs ys
  | otherwise = subS (x:xs) ys

eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (SetI xs) (SetI ys) = xs == ys

leqSet :: Eq a => Set a -> Set a -> Bool
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
