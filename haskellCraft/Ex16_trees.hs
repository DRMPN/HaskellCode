module Tree
  ( Tree,
    nil,      -- Tree a
    isNil,    -- Tree a -> Bool
    isNode,   -- Tree a -> Bool
    leftSub,  -- Tree a -> Tree a
    rightSub, -- Tree a -> Tree a
    treeVal,  -- Tree a -> a
    insTree,  -- Ord a => a -> Tree a -> Tree a
    delete,   -- Ord a => a -> Tree a -> Tree a
    minTree)  -- Ord a => Tree a -> Maybe a
  where

import Data.List (sort)

-- TODO make instances
data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq, Ord, Show)

nil :: Tree a
nil = Nil


isNil, isNode :: Tree a -> Bool

isNil Nil = True
isNil _ = False

isNode (Node _ _ _) = True
isNode _ = False


leftSub, rightSub :: Tree a -> Tree a

leftSub Nil = error "No leftSub"
leftSub (Node _ t1 _) = t1

rightSub Nil = error "No rightSub"
rightSub (Node _ _ t2) = t2


treeVal :: Tree a -> a
treeVal Nil = error "No value"
treeVal (Node v _ _) = v

insTree :: Ord a => a -> Tree a -> Tree a
insTree val Nil = Nil
insTree val (Node v t1 t2)
  | val == v = Node v t1 t2
  | val > v = Node v t1 (insTree val t2)
  | val < v = Node v (insTree val t1) t2

delete :: Ord a => a -> Tree a -> Tree a
delete val (Node v t1 t2)
  | val < v = Node v (delete val t1) t2
  | val > v = Node v t1 (delete val t2)
  | isNil t2 = t1
  | isNil t1 = t2
  | otherwise = join t1 t2

minTree :: Ord a => Tree a -> Maybe a
minTree t
  | isNil t = Nothing
  | isNil t1 = Just v
  | otherwise = minTree t1
  where
    t1 = leftSub t
    v = treeVal t

-- Join is an auxiliary function, used in delete
-- it is not exported.
join :: Ord a => Tree a -> Tree a -> Tree a
join t1 t2
  = Node mini t1 newt
  where
    (Just mini) = minTree t2
    newt = delete mini t2

-- find nth element of a tree
indexT :: Int -> Tree a -> a
indexT n t
  | isNil t = error "empty Tree"
  | n < st1 = indexT n t1
  | n == st1 = v
  | otherwise = indexT (n-st1-1) t2
  where
    v = treeVal t
    t1 = leftSub t
    t2 = rightSub t
    st1 = size t1

size :: Tree a -> Int
size t
  | isNil t = 0
  | otherwise = 1 + size (leftSub t) + size (rightSub t)


-- Think if making the size operation more efficent
-- by changing the implementation of the Tree

data Stree a = Snil | Snode a Int (Stree a) (Stree a)

-- Exercise 16.27
-- Redefine the functions of the Tree a signature
-- over the Stree implementation type

snil :: Stree a
snil = Snil

isSnil, isSnode :: Stree a -> Bool
isSnil Snil = True
isSnil _ = False

isSnode (Snode _ _ _ _) = True
isSnode _ = False

leftSubS, rightSubS :: Stree a -> Stree a
leftSubS t = case t of
  Snil -> error "Empty Stree"
  Snode _ _ t1 _ -> t1

rightSubS t = case t of
  Snil -> error "Empty Stree"
  Snode _ _ _ t2 -> t2

sTreeVal :: Stree a -> a
sTreeVal Snil = error "Empty Stree"
sTreeVal (Snode v _ _ _) = v

insStree :: Ord a => a -> Stree a -> Stree a
insStree val Snil = (Snode val 1 Snil Snil)
insStree val (Snode v n st1 st2)
  | v == val = Snode v n st1 st2
  | val > v = Snode v (1 + sizeS st1 + sizeS st2) st1 nst2
  | val < v = Snode v (1 + sizeS nst1 + sizeS st2) nst1 st2
    where
      nst1 = insStree val st1
      nst2 = insStree val st2

sizeS Snil = 0
sizeS (Snode _ n _ _) = n

deleteS :: Ord a => a -> Stree a -> Stree a
deleteS val (Snode v t1 t2)
  | val < v = Snode v (deleteS val t1) t2
  | val > v = Snode v t1 (deleteS val t2)
  | isSnil t2 = t1
  | isSnil t1 = t2
  | otherwise = joinS t1 t2

joinS :: Ord a => Stree a -> Stree a -> Stree a
joinS t1 t2
  = Snode mini t1 newt
  where
    (Just mini) = minStree t2
    newt = deleteS mini t2

minTreeS :: Ord a => Stree a -> Maybe a
minTreeS t
  | isNil t = Nothing
  | isNil t1 = Just v
  | otherwise = minTreeS t1
  where
    t1 = leftSubS t
    v = sTreeVal t

-- Exercise 16.25
-- Explain how would you test the implementation of the functions over search trees
-- 1) I would make 2 test trees, one is empty, second contains 2 or more nodes
-- 2) Firstly, test simple functions
-- 3) Compare your desired result and an output
--    If output is not correct to expected result, then find bugs in functions or in test values
--    If everything works, go it again with larger funcitons
-- 4) If something is not working correctly, and I cannot explain why, I'll do it step by step
--    through lambda funcitons in a shell
-- 5) Is all test passed? Then, no bugs has founded

-- Exercise 16.26
-- Define the functions

-- test values
testTreeOne :: Tree Int
testTreeOne = Node 2
  (Node 1 (Node 0 Nil Nil) (Nil))
  (Node 3 (Nil) (Node 6 Nil Nil))

testTreeTwo :: Fractional a => Tree a
testTreeTwo = Node 2.5 (Node 3.5 (Node 1.5 Nil Nil) Nil) (Node 7.5 (Node 5 Nil Nil) (Node 11 (Node 6.6 Nil Nil) Nil))

-- 1) Funciton
  -- successor of v in tree t is the smallest value t
  -- larger than v
successor :: Ord a => a -> Tree a -> Maybe a
successor value tree
  = takeSucc value . sort $ loSucc value tree
  where
    -- make a list of numbers that are bigger then given value
    loSucc v Nil = []
    loSucc v (Node val t1 t2)
      | val >= v = (loSucc v t1) ++ [val] ++ (loSucc v t2)
      | otherwise = (loSucc v t1) ++ (loSucc v t2)
    -- take successor of a list
    takeSucc v (x:x1:xs)
      | x > v = Just x
      | otherwise = Just x1
    takeSucc v _ = Nothing

-- 2) Function
  -- Closest value to v in a numerical tree t is a value in t
  -- which has the smallest difference from v
closest :: Int -> Tree Int -> Int
closest v Nil = v
closest v t = smallDiff bnot bnot v $ allNums
  where
    -- Biggest number of the given tree
    bnot = bigNum allNums
    bigNum l = foldr1 (\v svd -> if svd < v then v else svd) l
    -- List of all numbers of the tree
    allNums = allItems t
    -- Find a closest value in the list to a given number
    smallDiff _ svd _ [] = svd
    smallDiff curDif svd v (x:xs)
      | diff < curDif = smallDiff diff x v xs
      | otherwise = smallDiff curDif svd v xs
      where
        diff = abs $ v - abs x

-- Useful functions so I decided to give a normal definition
allItems :: Tree a -> [a]
allItems t = case t of
  Nil -> []
  Node v t1 t2 -> [v] ++ (allItems t1) ++ (allItems t2)

