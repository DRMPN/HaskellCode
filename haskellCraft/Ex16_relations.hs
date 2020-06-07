module Ex16_relations
-- TODO hide
where
----------------------------Imports---------------------------
import Ex16_sets

--------------------------------------------------------------
----------------------------Hardcode--------------------------

type People = String
isParent :: Set (People,People)
isParent = SetI [("Ben","Sue"), ("Ben","Leo"),("Sue", "Joe")]

--------------------------------------------------------------
----------------------------Funcitons-------------------------
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
