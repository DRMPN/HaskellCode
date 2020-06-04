module Decoding where

import Types

decodeMessage :: Tree -> HCode -> [Char]
decodeMessage tr = decodeByt tr
  where
    decodeByt (Node n t1 t2) (L:rest) =
      decodeByt t1 rest
    decodeByt (Node n t1 t2) (R:rest) =
      decodeByt t2 rest
    decodeByt (Leaf c n) rest =
      c : decodeByt tr rest
    decodeByt t [] = []

exam1 = Node 0
        (Leaf 'a' 0)
        (Node 0
          (Leaf 'b' 0)
          (Leaf 't' 0))
mess1 = [R,L,L,R,R,R,R,L,R,R]

-- Exercise 15.6
-- Complete the calculation of decodeMessage exam1 mess1
{-
decodeMessage exam1 mess1
decodeByt exam1 mess1
decodeByt Node 0 (Leaf 'a' 0) (Node 0 (Leaf 'b' 0) (Leaf 't' 0))
          [L,L,R,R,R,R,L,R,R]
decodeByt (Node 0 (Leaf 'b' 0) (Leaf 't' 0))
          [L,R,R,R,R,L,R,R]
decodeByt (Leaf 'b' 0) [L,R,R,R,R,L,R,R]
b : decodeByt tr [L,R,R,R,R,L,R,R]
b : decodeByt Node 0 (Leaf 'a' 0) [R,R,R,R,L,R,R]
b : deodeByt Leaf 'a' 0 [R,R,R,R,L,R,R]
b : a : decode tr [R,R,R,R,L,R,R]
b : a : decode Node 0 (Leaf 'a' 0)
               (Node 0 (Leaf 'b' 0) (Leaf 't' 0))
               [R,R,R,L,R,R]
b : a : decode Node 0 (Leaf 'b' 0) (Leaf 't' 0)
               [R,R,L,R,R]
b : a : t : decode tr [R,R,L,R,R]
-- same procces as above
b : a : t : t : decode tr [L,R,R]
b : a : t : t : decode Node 0 (Leaf 'a' 0) (...)
                [R,R]
b : a : t : t : decode Leaf 'a' 0 [R,R]
b : a : t : t : a : decode tr [R,R]
--
b : a : t : t : a : t : decode tr []
b : a : t : t : a : t : []
"battat"
-}

