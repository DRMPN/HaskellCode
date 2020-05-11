module Store
  ( Store,
    initial, -- Store
    value,   -- Store -> Var -> Int
    update,  -- Store -> Var -> Int -> Store
  ) where

import Data.List (sort)

type Var = Int

newtype Store = Sto [(Int, Var)]

-- these instances cannot be hidden
instance Eq Store where
  (Sto sto1) == (Sto sto2) = sto1 == sto2

instance Show Store where
  show (Sto sto) = show sto

init :: [(Int, Var)]
init = []

val :: [(Int,Var)] -> Var -> Int
val [] v = 0
val ((n,w):sto) v
  | v == w = n
  | otherwise = val sto v

initial :: Store
initial = Sto []

value :: Store -> Var -> Int
value (Sto []) v = 0
value (Sto ((n,w):sto)) v
  | v == w = n
  | otherwise = value (Sto sto) v

update :: Store -> Var -> Int -> Store
update (Sto sto) v n = Sto ((n,v):sto)

-- Ex 16.1
-- Give an inplementation of Store using lists whose entries
-- are ordered according to the variable names
updateSort :: Store -> Var -> Int -> Store
updateSort (Sto sto) v n = Sto $ sort ((n,v):sto)

-- Ex 16.3
-- Ex 16.4
newtype Test = Tes [(Maybe Int, Var)]

testValue :: Test -> Var -> Maybe Int
testValue (Tes []) v = Nothing
testValue (Tes ((n,w):tes)) v
  | null n = n
  | v == w = n
  | otherwise = testValue (Tes tes) v

-- Ex 16.13
-- Are all the operations in the Tree a signature necessary?
-- Identify those which can be implemented using the other
-- operations of the signature.
{-
We can use nil to check if a tree is empty or if a tree is a node,
therefore we would omit functions like isNil and isNode
testTree :: Tree
nil == testTree
True -> It's empty
False -> It's a node
If we won't use treeVal function to get a value from a tree, then
it could be ommited

leftSub or rightSub could be defined as global or local funcrions
and used to implement other functions like insTree or delete

I'm not sure about minTree functions
-}

-- Ex 16.14
-- Design a signature for an abstract type of library databases.
{-
In my opinions we can use simple implementation like this:
newtype Database a = Db [a]

so it could be a list of customers/employees/items etc
-}
