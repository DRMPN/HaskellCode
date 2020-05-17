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

data Tree a = Nil | Node a (Tree a) (Tree a)

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
