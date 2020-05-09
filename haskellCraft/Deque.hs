-- !!! Using lists to implement a Deque is not very efficent
module Deque
  (Deque,
   emptyDq,   -- Deque a
   isEmptyDq, -- Deque a -> Bool
   frontDq,   -- Deque a -> a
   backDq,    -- Deque a -> a
   remFront,  -- Deque a -> Deque a
   remBack,   -- Deque a -> Deque a
   addFront,  -- Deque a -> Deque a
   addBack    -- Dequq a -> Deque a
  ) where

newtype Deque a = Deque [a]
  deriving Show

emptyDq = Deque []

isEmptyDq dq = case dq of
  Deque [] -> True
  _ -> False

frontDq dq = case dq of
  Deque [] -> error "Front element is missing"
  Deque (x:xs) -> x

backDq dq = case dq of
  Deque [] -> error "Back element is missing"
  Deque q@(x:xs) -> last q

remFront dq = case dq of
  Deque [] -> error "There's nothing to remove"
  Deque (x:xs) -> Deque xs

remBack dq = case dq of
  Deque [] -> error "There's nothing to remove"
  Deque q@(x:xs) -> Deque $ init q

addFront x (Deque xs) = Deque (x:xs)

addBack x (Deque xs) = Deque (xs ++ [x])
