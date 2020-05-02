module MakeTree where

import Types

makeTree :: [ (Char,Int)] -> Tree
makeTree = makeCodes . toTreeList

toTreeList :: [ (Char, Int)] -> [Tree]
toTreeList = map (uncurry Leaf)

makeCodes :: [Tree] -> Tree
makeCodes [t] = t
makeCodes ts = makeCodes (amalgamate ts)

-- TODO definition of insTree is similar as in insertSort
amalgamate :: [Tree] -> [Tree]
amalgamate (t1:t2:ts) = insTree (pair t1 t2) ts

pair :: Tree -> Tree -> Tree
pair t1 t2 = Node (v1+v2) t1 t2
  where
    v1 = value t1
    v2 = value t2

value :: Tree -> Int
value (Leaf _ n) = n
value (Node n _ _) = n
