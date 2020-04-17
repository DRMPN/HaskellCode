import qualified Data.Char as C
import qualified Data.List as L

import Ex5_10

-- 5.1
maxOccurs :: Int -> Int -> (Int,Int)
maxOccurs a b =
  let occ m n = if m == n then 2 else 1
  in (max a b, occ a b)

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs a b c =
  let howManyNotUnique m n p
        | threeEqual m n p = 3
        | threeNotEqual m n p = 1
        | otherwise = 2

      threeEqual a b c =
        a==b && b==c

      threeNotEqual a b c =
        a/=b && a/=c && b/=c

      maxThree a b c
        | a>=b && a>=c = a
        | b>=c = b
        | otherwise = c

  in (maxThree a b c, howManyNotUnique a b c)

-- 5.2
orderTriple :: (Int,Int,Int) -> (Int,Int,Int)
orderTriple (f,s,t) =
  let maxThree a b c
        | a>=b && a>=c = a
        | b>=c = b
        | otherwise = c

      middle x y z
        | between y x z      = x
        | between x y z      = y
        | otherwise          = z

      between m n p = (n >= m && n <= p) || (n <= m && n >= p)

      minThree a b c
        | a<=b && a<=c = a
        | b<=c = b
        | otherwise = c
  in
    (minThree f s t, middle f s t, maxThree f s t)

-- 5.3

-- [(3,x), (7,y), (6, =)]
findXaxis :: [(Int,Char)] -> (Int, Int)
findXaxis [(a,_),_,(b,_)] =
  (b `div` a , 0)

-- 5.4

type Equation = [PartOfEq]
type PartOfEq = (Int,Char)
type Coordinates = (Int, Int)

findXaxis1 :: Equation -> Coordinates
findXaxis1 [(a,_),_,(b,_)] =
  (b `div` a , 0)

-- 5.8

doubleAll :: [Int] -> [Int]
doubleAll xs = [ x*2 | x <- xs]

-- 5.9

capitalize :: String -> String
capitalize xs = [C.toUpper x | x <- xs]

capitalizeLetters :: String -> String
capitalizeLetters xs = [C.toUpper x | x <- xs , C.isLetter x]

-- 5.10

-- in Ex5_10.hs

-- 5.11

{-

matches 1 [1,2,1,4,5,1] --i [1,1,1]
matches 1 [2,3,4,6] []

-}

matches :: Int -> [Int] -> [Int]
matches n list = foldr (\x acc -> if x == n then x:acc else acc) [] list

{-

elem I [1,2,1,4,5,1] --t True
elem 1 [2,3,4,6] --t False

-}

elem' :: Int -> [Int] -> Bool
elem' x xs = (>0) . length . matches x $ xs

-- 5.13

type Person = String
type Book = String

type Database = [(Person, Book)]
exampleBase :: Database
exampleBase = [("Alice" , "Tintin") , ("Anna" , "Little Women"),
               ("Alice" , "Asterix") , ("Rory" , "Tintin")]

books :: Database -> Person -> [Book]
books db prsn = [book | (person,book) <- db , person == prsn]

borrowers :: Database -> Book -> [Person]
borrowers db bk = [person | (person,book) <- db , book == bk]

numBorrowed :: Database -> Person -> Int
numBorrowed db prsn = length . books db $ prsn

isBorrowed :: Database -> Book -> Bool
isBorrowed db bk = (>0) . length . borrowers db $ bk

makeLoan :: Database -> Person -> Book -> Database
makeLoan db prsn bk = [(prsn , bk)] ++ db

returnLoan :: Database -> Person -> Book -> Database
returnLoan db prsn bk = [pair | pair <- db , pair /= (prsn , bk)]

-- 5.15
-- will do something like this
-- but concat is kinda crap-shit bycicle
-- !!! in need of reforge :
                        -- refactor list of lists or make cool filter

type NewDB = [(Person , [Book])]

newBooks :: NewDB -> Person -> [Book]
newBooks db prsn = [concat $ books | (person,books) <- db , person == prsn]

-- 5.16

{-
snd :: (a,b) -> b
snd (x,y) = y

sing :: a -> [a]
sing x = [x]
-}

-- 5.17

{-
bc input and output is the same
[[a]] -> [[a]]

more general form is [a] -> [a]
-}

-- 5.18

{-
general form would look like this
((a,a) , a) -> (a , (a,a))

bu whad bout thAt
(a)->(a)
or even THAT
a -> a
???
-}

-- 5.20
-- !!! in need of reforge

romanDigit :: Char -> String
romanDigit a = toRoman . C.digitToInt $ a

toRoman :: Int -> String
toRoman n
  | 0 < n && n < 4 = replicate n 'I'
  | n == 4 = "IV"
  | 5 < n && n < 9 = "V"++(replicate (n - 5) 'I')
  | n == 9 = "IX"
  | otherwise = "V"

-- 5.21
-- !!! in need of reforge
onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a++"\n"++b++"\n"++c++"\n"

-- 5.22

onSeparateLines :: [String] -> String
onSeparateLines = concat . map (++"\n")

-- 5.23

duplicate :: String -> Int -> String
duplicate str n
  | n < 0 = ""
  | n ==1  = str
  | otherwise = concat . replicate n $ str

-- 5.24

linelength = 12

strlength :: String -> Bool
strlength = (==linelength) . length

pushRight :: String -> String
pushRight str = if strlength str
  then str else (concat $ replicate ((-) linelength $ length str) " ")++str

-- 5.25

{-
what if the linelength is less than length of the string
-}

-- 5.26

{-

> putStr (fibTable 6)

n    fib n
0        0
1        1
2        1
3        2
4        3
5        5
6        8

-should use \t and \n ---- used
-test how it works ---- doesn't work

-"n" ++ pushRight "fib n" ---- works perfectly

-implement fib function ++ fib function that produces tuple? list of tuple?
-test simple function of \t \n with two variables
-bild mega function

-}

fibTable :: Int -> String
fibTable n = "kek"

fib :: Int -> Int
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n-2) + fib (n-1)


fibTest :: Int -> IO()
fibTest n =
  helper n 0 ("n" ++ (pushRight $ "fib n") ++ "\n")

helper :: Int -> Int -> String -> IO()
helper n num los =
  let saved = (num, fib num)
  in
    if num > n
    then putStrLn los
    else helper n (num+1) (los ++ ((show $ fst saved) ++ (pushRight . show $ snd saved) ++ "\n"))


{-

  let saved = (n, fib n)
  in (++) (show $ fst saved) $ (pushRight . show $ snd saved)
     (show $ fst saved) ++ (pushRight . show $ snd saved) ++ "\n"


-}

  -- 5.27

-- reforge database function to deal with more readable output
