-- TODO remove overlapping warning

-- !!! Using lists to implement a Deque is not very efficent
module Deque
  (Deque,
   emptyDq, -- Deque a
   isEmptyDq, -- Deque a -> Bool
   frontDq,
   backDq,
   remFront,
   remBack,
   addFront,
   addBack) where

newtype Deque a = Deque [a]

emptyDq = Deque []

isEmptyDq (Deque []) = True
isEmptyDq _ = False

frontDq (Deque (x:xs)) = x
frontDq (Deque []) = error "Front element is missing"

backDq (Deque a) = last a
backDq (Deque []) = error "Back element is missing"

remFront (Deque (x:xs)) = Deque xs
remFront (Deque []) = error "There's nothing to remove"

remBack (Deque a) = init a
remBack (Deque []) = error "There's nothing to remove"

addFront x (Deque xs) = Deque (x:xs)

addBack x (Deque xs) = Deque (xs ++ [x])
