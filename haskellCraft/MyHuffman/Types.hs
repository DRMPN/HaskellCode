module Types where
{-
or

module Types ( Tree (..) ,
               Bit (..) ,
               HCode ,
               Table ) where
-}


-- The types of bits, Huffman codes and tables of Huffman codes.
data Bit = L | R deriving (Eq,Show)

type HCode = [Bit]

type Table = [ (Char, HCode) ]

-- Trees to represent the relative frequencies of characters
-- and therefore the Huffman codes.
data Tree = Leaf Char Int |
            Node Int Tree Tree
