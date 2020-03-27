import Data.List
encode  :: (Eq a) => [a] -> [(Int,a)]
encode xs = map (\s -> (length s, head s)) (group xs)

-- Problem 11
data ListItem a = Single a | Multiple Int a
  deriving (Show)

encodeModified  :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
  where encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x

-- Problem 12
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
  where decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

