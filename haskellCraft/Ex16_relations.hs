module Ex16_relations
-- TODO hide
where
--------------------------Imports-----------------------------
import Ex16_sets
import Data.List (nub)

--------------------------------------------------------------
--------------------------Hardcode----------------------------

type People = String
isParent :: Set (People,People)
isParent = SetI [("Ben","Sue"), ("Ben","Leo"),("Sue", "Joe")]

--------------------------------------------------------------
--------------------------Parents-----------------------------
type Relation a = Set (a,a)

image :: Ord a => Relation a -> a -> Set a
image rel val = mapSet snd (filterSet ((==val) . fst) rel)

setImage :: Ord a => Relation a -> Set a -> Set a
setImage rel = unionSet . mapSet (image rel)

unionSet :: Ord a => Set (Set a) -> Set a
unionSet = foldSet union empty

addImage :: Ord a => Relation a -> Set a -> Set a
addImage rel st = st `union` setImage rel st

addChildren :: Set People -> Set People
addChildren = addImage isParent

compose :: Ord a => Relation a -> Relation a -> Relation a
compose rel1 rel2
  = mapSet outer (filterSet equals (setProduct rel1 rel2))
  where
    equals ((a,b),(c,d)) = (b==c)
    outer ((a,b),(c,d)) = (a,d)

setProduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
setProduct st1 st2 = unionSet (mapSet (adjoin st1) st2)

adjoin :: (Ord a, Ord b) => Set a -> b -> Set (a,b)
adjoin st el = mapSet (addEl el) st
  where
    addEl el el' = (el', el)

tClosure :: Ord a => Relation a -> Relation a
tClosure rel = limit addGen rel
  where
    addGen rel' = rel' `union` (rel' `compose` rel)

limit :: Eq a => (a -> a) -> a -> a
limit f x
  | x == next = x
  | otherwise = limit f next
  where
    next = f x

--------------------------------------------------------------
--------------------------Graphs------------------------------
graph1 = SetI [(1,2),(1,3),(3,2),(3,4),(4,2),(2,4)]

connect :: Ord a => Relation a -> Relation a
connect rel = clos `inter` solc
  where
    clos = tClosure rel
    solc = inverse clos

inverse :: Ord a => Relation a -> Relation a
inverse = mapSet swap
  where swap (x,y) = (y,x)

classes :: Ord a => Relation a -> Set (Set a)
classes rel =
  limit (addImages rel) start
  where start = mapSet sing (eles rel)

eles :: Ord a => Relation a -> Set a
eles rel = mapSet fst rel `union` mapSet snd rel

addImages :: Ord a => Relation a -> Set (Set a) -> Set (Set a)
addImages rel = mapSet (addImage rel)

-- Searching in graphs
depthFirst :: Ord a => Relation a -> a -> [a]
depthFirst rel v = depthSearch rel v []

depthSearch :: Ord a => Relation a -> a -> [a] -> [a]
depthSearch rel v used
  = v : depthList rel (findDescs rel used' v) used'
  where used' = v:used

depthList :: Ord a => Relation a -> [a] -> [a] -> [a]
depthList rel [] used = []
depthList rel (val:rest) used
  = next ++ depthList rel rest (used ++ next)
  where
    next = if elem val used
           then []
           else depthSearch rel val used

breadthFirst :: Ord a => Relation a -> a -> [a]
breadthFirst rel val = limit step start
  where
    start = [val]
    step xs = xs ++ nub (concat (map (findDescs rel xs) xs))

findDescs :: Ord a => Relation a -> [a] -> a -> [a]
findDescs rel xs v = flatten (newDescs rel (makeSet xs) v)
-- find all the descendants of a node which have not been
-- visited so far
newDescs :: Ord a => Relation a -> Set a -> a -> Set a
newDescs rel st v = image rel v `diff` st

flatten :: Set a -> [a]
flatten (SetI xs) = xs

-- returns minimum of a non-empty set
minSet :: Set a -> Maybe a
minSet = undefined

-- Exercise 16.41
-- Calculate
{-
classes (connect graph1) -> SetI []
classes (connect graph2) -> SetI []

where graph2 = graph1 `union` SetI [(4,3)]
-}

-- Ex. 16.42
-- Give calculations of
{-
graph2 = SetI [(1,2),(1,3),(3,2),(3,4),(4,2),(4,3)]

breadthFirst graph2 1 -> [1,2,3,4]

depthFirst graph2 1   -> [1,2,4,3]
-}

-- Ex. 16.43
-- Give a function
{-
distance graph1 1 4 -> 2
  Works
distance 4 1 -> 0
  Inf. loop between 2 and 4
-}

--distance :: Eq a => Relation a -> a -> a -> Int
--distance (SetI xs) a b = dist xs a b 0

--dist :: [a] -> a -> a
dist [] _ _ val = val
dist t@(x:xs) a b val
  | elem (a,b) t = val + 1
  | otherwise = cont t b (val+1) $ filter (\(x,y) -> x == a) t

cont t b val xs = again t b val (map (\(a,b) -> b) xs)

again rel b val (x:xs) = dist rel x b val

-- test
unset (SetI xs) = xs

