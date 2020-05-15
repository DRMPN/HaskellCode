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
