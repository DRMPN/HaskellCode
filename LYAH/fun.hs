import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

doubleMe x = x + x
doubleUs x y = doubleMe x  + doubleMe y

doubleSmallNumber x =
  if x>=100
  then x
  else doubleMe x

boomBangs xs = [if x<10 then "BOOM!" else "BANG!" |x<-xs, odd x]

length' xs = sum [1 | _ <-xs]

--let nouns = ["hobo", "frog", "pope"]
--let adjectives = ["lazy", "grouchy", "scheming"]
--[adjectives ++ " " ++ noun | adjectives <- adjectives, noun <- nouns]

removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

--let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
--[ [ x | x <- xs, even x ] | xs <- xxs]

--Tuples
--let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
--let triangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], c^2==a^2+b^2 ]
--let triangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], c^2==a^2+b^2, a+b+c==24 ]

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky 777 = "LUCKY NUMBER SEVEN HUNDRED AND SEVENTY SEVEN"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- LIDL version --
-- addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)

-- a OK version --
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
-- " " - need fix
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale, congratulations!"
    where
      bmi = weight / height ^ 2
      (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

-- another way (hard?)
-- max' :: (Ord a) => a -> a -> a

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a > b = GT
    | a < b = LT
    | otherwise = EQ

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where
      (f:_) = firstname
      (l:_) = lastname

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
    where
      bmi weight height = weight / height ^ 2

--calcBmis :: (RealFloat a) => [(a, a)] -> [a]
--calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder h r =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in  sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of (x:_) -> x
                       [] -> error "No head for empty lists!"

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallSorted   = quicksort [a | a <- xs, a <= x]
      biggerSorted  = quicksort [a | a <- xs, a > x]
  in smallSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
-- *Main> let multTwoWithNine = multThree 9
-- *Main> multTwoWithNine 2 3
-- *Main> 54
-- multTwoWithNine 2 3 = multThree 9 2 3
-- *Main> let multWithEighteen = multTwoWithNine 2
-- *Main> multWithEighteen 10
-- *Main> 180
-- multWithEighteen 10 = multTwoWithNine 2 10 = multThree 9 2 10

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
-- divideByTen 200 = (/10) 200 = 200/10

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])
-- isUpperAlphanum 'A' = (`elem` ['A'..'Z']) 'A' = 'A' `elem` ['A'..'Z']

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' ::(a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

flip' :: (a->b->c) -> (b->a->c)
flip' f y x = f x y

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerSorted = quicksort' (filter (<=x) xs)
      biggerSorted  = quicksort' (filter (>x)  xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- takeWhile (/=' ') "elephants know how to party"
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- sum (takeWhile (<10000) [n^2 | n<- [1..], odd (n^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div`2)
  | odd n  = n:chain (n*3 + 1)

numLongChains:: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- let listOfFuns - map (*) [0..]
-- (listOfFuns !! 4) 5
-- 20

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- map (\x -> x+3) [1,3,7] the same shit as map (+3) [1,3,7]
-- zipWith (\a b -> (a * 30 + 3) / b) [3,2,1] [1,2,3]
-- map (\(a,b) -> a + b) [(1,2),(3,4),(5,6)]

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- a kek version
--sum'' :: (Num a) => [a] -> a
--sum'' xs = foldl (\acc x -> acc + x) 0 xs

-- a ok version
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x ==y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
-- map' f x = foldl (\acc x -> acc ++ [f x]) [] xs (++ is much more expensive)

maximum''' :: (Ord a) => [a] -> a
maximum''' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head''' :: [a] -> a
head''' = foldr1 (\x _ -> x)

last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)

-- scanl1 (\acc x -> if x > acc then x else acc)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sum (map sqrt [1..130])
-- sum $ map sqrt [1..130]

-- sum (filter (>10) (map (*2) [2..10])) -- f (g (z x))
-- sum $ filter (>10) $ map (*2) [2..10] -- f $ g $ z x

-- remember (.) shit
-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- f (g (z x)) == (f . g . z) x
-- map (\x -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]]
-- map (negate . sum . tail)          [[1..5], [3..6], [1..7]]

-- sum (replicate 5 (max 6.7 8.9)) same as (sum . replicate 5 . max 6.7) 8.9
           -- or as sum . replicate 5 . max 6.7 $ 8.9
-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- replicate 100 . product . map (*3) . zipWith  max [1,2,3,4,5] $ [4,5,6,7,8]

-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn = ceiling . negate . tan . cos . max 50

-- putStrLn (show (1 + 1))
  -- 1. (1+1) - doesn't have an intput so can't use .
  -- 2. show can accept Int and produce String
  -- 3. putStrLn can accept String and produce IO ()
-- (putStrLn . show) (1 + 1)
-- putStrLn . show $ 1 + 1

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
  -- for nuuubs:
  -- oddSquareSum :: Integer
  -- oddSquareSum =
  --     let oddSquares = filter odd $ map (^2) [1..]
  --         belowLimit = takeWhile (<10000) oddSquares
  --     in sum belowLimit

-- after import Data.List
-- to ghci :m + <file name>

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- to import only few functions do this:
-- import Data.List (nub, sort)
-- to except function from module do this:
-- import Data.List hidding (nub)

-- to reference Data.Map's filter, we just use M.filter
-- dropWhile (<3) [1,,2,2,3,4,5,3,2,2,1]

-- let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
-- head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
-- find (\(val,y,m,d) -> val > 1000) stock

-- break (==4) [1,2,3,4,5,6,7]     break       p
-- span  (/=4) [1,2,3,4,5,6,7]     span (not . p)

-- how many elements in the list and group them together
-- map (\l@(x:xs) -> (x,length 1)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- zipWith3 (\ x y z -> x + y + z)  [1,2,3] [4,5,2,2] [2,2,3]
-- zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted
-- cowboy composition map (chr . (+ shift) . ord) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg


-- crash if empty
--findKey :: (Eq k) => k -> [(k,v)] -> v
--findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs


findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing


phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

-- Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- what a kek way data Person = Person String String Int Float String String deriving (Show)

data Person = Person
              { firstName :: String
              , lastName :: String
              , age :: Int
              , height :: Float
              , phoneNumber :: String
              , flavor :: String
              } deriving (Show)

data Car a b c = Car
           { company :: a
           , model :: b
           , year :: c
           } deriving (Show)

tellCar :: (Show a) => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n


type Name = String

type PhoneNumber = String

type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name,number) `elem` book

type AssocList k v = [(k,v)]
-- (Eq k) => k -> AssocList k v -> Maybe v


data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- lockerLookup 101 lockers


-- data List a = Empty | Cons a (List a) deriving (Show,Read,Eq,Ord)

infixr 5 :-:

data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- just wrote a :-: (List a) instead of Cons a (List a)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- Binary Search Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- Remember, pretty much everything that traverses a list one by one and then returns some sort of value can be implemented with a fold!
-- let nums = [8,6,4,1,7,3,5]
-- let numsTree = foldr treeInsert EmptyTree nums


-- Traffic light with class

data TrafficLights = Red | Yellow | Green

instance Eq TrafficLights where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLights where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

-- weakly typed JavaScript behaivor
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe m) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = True
  yesno _ = True

instance YesNo TrafficLights where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- yesnoIf (Just 500) "YEAH!" "NO!"


instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

-- fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])

