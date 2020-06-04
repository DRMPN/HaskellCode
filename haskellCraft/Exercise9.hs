
-- :set +s

-- 9.1

{-
doubleAll 2 1 7

doubleAll as list comprehension
[2*x | x <- [2,1,7]]
[2*2 | 2 <- [1,7]]
[2*2 | 1 <- [7]]
[2*7 | 7 <- []]
[2*[] | [] <- []]
[4,2,14]

doubleAll as resursion
doubleAll [2,1,7] = 4 : doubleAll [1,7] = 4 : 2 : doubleAll [7] =
4 : 2 : 14 : doubleAll [] = 4 : 2 : 14 : [] = [4,2,14]

doubleAll as map
map doubleAll [2,1,7] = doubleAll [2,1,7] -> doubleAll [1,7] -> doubleAll [7] -> doubleAll [] = [4,2,14]
-}

-- 9.2
-- implement length using sum and map
myLength :: [a] -> Int
myLength = sum . map (\n -> 1)

-- 9.3
{-
redefine it using map before filter
addUp ns = filter greaterOne (map addOne ns)
  where greaterOne n = n > 1
        addOne n = n + 1
-}

addUp :: (Num a, Ord a) => [a] -> [a]
addUp = map (\n -> n+1) . filter (>0)

-- 9.4
{-
describe effect of map f (map g xs)
(x:xs)
f (g x) -> f (g xs)
-}

-- 9.5
{-
what is the effect of filter (>1) (filter (<10) xs)
the effect is : 1<xs<10
-}

-- 9.6
-- squares all items in a list
squareAll :: (Num a) => [a] -> [a]
squareAll = map (^2)

-- squares items in a list and then sum them all
sumSquares :: (Num a) => [a] -> a
sumSquares = sum . squareAll

-- checks wheter all items of the list are greater than ten
gt10 :: [Int] -> Bool
gt10 = null . filter (<10)

-- 9.7
-- can't understand, probably will not do this ever xD

-- 9.8
-- twice double 7 = 28
-- twice :: (a -> a) -> a -> a
twice f a = f . f $ a

-- 9.9
-- iter 3 f x = f (f (f x))
-- iter 0 f x = x
iter 0 _ x = x
iter n f x = iter (n-1) f (f x)

-- 9.10
-- input n return 2^n
twoInN :: Int -> Int
twoInN n = iter n (*2) 1

-- 9.11
-- define sum of squares of natural numbers from 1 to n
-- using map and foldr

sumFold :: Int -> Int
sumFold n = foldr (+) 0 $ map (^2) [1..n]

-- 9.12
-- sum of squares of the positive integers in a list of integers
-- i think like this

sumPosInt :: [Int] -> Int
sumPosInt = foldr1 (+) . map (^2) . filter (>0)

-- 9.13
-- use foldr to give definition of : unZip , last , init

unzipFold :: [(a,b)] -> ([a],[b])
unzipFold = foldr (\n acc -> (fst n : fst acc, snd n : snd acc)) ([],[])

lastFold :: [a] -> a
lastFold = foldr1 (\_ acc -> acc)

-- skipped init bc dont want to implement it neither through length nor maybe nor helper function with empty case

-- 9.14
-- mystery func works like id

-- 9.15
{-
ununderstandabe shit
formatList :: (a -> String) -> [a] -> String
formatList f xs = foldr (\n acc -> f n : acc) [] xs
-}

-- 9.16
-- filters first item in the list that satisfied to first param
-- filterFirst (==1) [2,3,4,1,5,6,1]
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst f xs =
  let helper _ [] _ = []
      helper func (x:xs) acc
        | func x = reverse acc ++ xs
        | otherwise = helper func xs (x:acc)
  in helper f xs []

-- 9.17
-- filters last element of the list that satisfied to first par
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast f xs = reverse $ filterFirst f $ reverse xs

-- 9.18
-- reforge supermarket billing

-- 9.19
{-
Give the type and definition of the generalization dropuntil of the function dropword.
dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
  | elem x whitespace = (x:xs)
  | otherwise = dropWord xs

something like this
im not sure btw
-}
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f [] = []
dropUntil f (x:xs)
  | f x = (x:xs)
  | otherwise = dropUntil f xs

-- 9.20
-- define dropSpace using dropUntil
dropSpace :: String -> String
dropSpace = dropUntil (/=' ')

-- define takeWhile using getUntil
getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p [] = []
getUntil p (x:xs)
  | p x = []
  | otherwise = x : getUntil p xs
-- !!!
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = []

-- 9.21
-- split string into lines using dropUntil getUntil
lineLength = 14

-- empty case?
spaceRemover :: String -> String
spaceRemover a = dropUntil (/=' ') a

-- !!! encapsulate
splitLine :: String -> String
splitLine a = func a [] lineLength

func :: String -> String -> Int -> String
func [] lines _ = lines
func list lines num
  | lenWord > num = func (word ++ rest) (lines++"\n") lineLength
  | otherwise = func (spaceRemover rest) (lines ++ word ++ " ") (num - lenWord)
  where
    word = getUntil (== ' ') list
    rest = dropUntil (== ' ') list
    lenWord = length word

-- 9.22
-- don't think that function getLine from chap7 has any polymorphic type bc it takes integer and list of string
-- if the function took a list of anything (or at least more then unicode)
-- then it could be more polymorphic but its not

-- 9.23
{-
give more generalization to functions
i'm not sure, but from my point of view functions should take text rather than string
-- cringe above

funcitons should take type [a] instead of String to be more polymorphic
-}
