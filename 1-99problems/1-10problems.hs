import Data.List
-- Problem 1
myLast :: [a] -> a
myLast = head . reverse

-- Problem 2
myButLast :: [a] -> a
myButLast = last . init

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt list num = last $ take num list
{-
elementAt :: [a] -> Int -> a
elementAt list i    = list !! (i-1)
-}

-- Problem 4
myLength :: (Enum a) => [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs
-- Using accumulator
myLength' :: [a] -> Int
myLength' list = myLength_acc list 0
   where myLength_acc [] n = n
         myLength_acc (_:xs) n = myLength_acc xs (n+1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse list = myAcc list []
  where myAcc [] acc = acc
        myAcc (x:xs) acc = myAcc xs (x:acc)

--init last

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == (reverse list)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress list = myAcc list []
  where myAcc [] acc = reverse acc
        myAcc (x:xs) acc = myAcc (filter (/=x) xs) (x:acc)

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) =
  let (first,rest) = span (== x) xs
  in (x:first) : pack rest

-- Problem 10
encode  :: (Eq a) => [a] -> [(Int,a)]
encode xs = map (\s -> (length s, head s)) (group xs)
