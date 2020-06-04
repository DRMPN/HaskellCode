module Exercise7 where

import Data.List (nub, delete)
import Data.Char (toLower)
import Chapter5
import Prelude hiding (Word, getLine)

firstDigit :: String -> Char
firstDigit st =
  case (digits st) of
    [] -> '\0'
    (x:_) -> x

-- 7.1
firstInteger :: [Int] -> Int
firstInteger ints =
  case ints of
    (x:y:_) -> x + y
    otherwise -> 0

-- 7.2
addFirstTwo :: [a] -> [a]
addFirstTwo xs =
  case xs of
    (a:b:_) -> [a,b]
    (x:_) -> [x]
    otherwise -> []

-- 7.3
firstInteger' :: [Int] -> Int
firstInteger' xs =
  if length xs >= 2
  then head xs + xs !! 1
  else 0

addFirstTwo' :: [a] -> [a]
addFirstTwo' xs
  | length xs >= 2 = head xs : xs !! 1 : []
  | null xs = []
  | otherwise = head xs : []

-- 7.4
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

-- 7.5
and' :: [Bool] -> Bool
and' [] = True
and' (a:as) = a && and' as

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs


iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)
  where
    ins b [] = [b]
    ins b (a:as)
      | b <= a = b:(a:as)
      | otherwise = a : ins b as

-- 7.6
elemNumRc :: Int -> [Int] -> Int
elemNumRc n [] = 0
elemNumRc n (x:xs)
  | n == x = 1 + elemNum n xs
  | otherwise = elemNumRc n xs

elemNum :: Int  -> [Int] -> Int
elemNum x xs = length $ filter (==x) xs

-- 7.7
unique :: [Int] -> [Int]
unique [] = []
unique (x:xs)
  | elem x xs = unique $ filter (/=x) xs
  | otherwise = x:unique xs

-- 7.8
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

unzip' :: [(a,b)] -> ([a], [b])
unzip' a = (map fst a, map snd a)

-- 7.9
minAndMaxList :: [Int] -> (Int,Int)
minAndMaxList a = (findMin a, findMax a)
  where findMin = foldr1 (\n acc -> if n > acc then acc else n)
        findMax = foldr1 (\n acc -> if n < acc then acc else n)

minAndMaxList' :: [Int] -> (Int,Int)
minAndMaxList' x = (head done, last done)
  where done = iSort x

-- 7.10 and 7.11
-- !!! reforge remove reverse
-- probably change function to fold

ins :: Ord a => a -> [a] -> [a]
ins a as = reversedIns a as
          where reversedIns a as =
                  let
                    help item [] [] = item:[]
                    help item (x:xs) list
                      | item < x = list ++ (x:xs) ++ [item]
                      | otherwise = help item xs list ++ [x]
                  in help a as []

-- 7.12
{-
test data for duplicate removing nub $ iSort
1) on empty list
2) list with no duplicates
3) list with two duplicates
-}

-- 7.13
-- !!! TODO
-- list of tuples to lexicographic order i.e (2,73) < (3,0) < (3,2)
lexicOrd [] = []
lexicOrd (x:xs) = [y | y<-xs , (fst y <= fst x && snd y <= snd x)] ++ [x] ++ lexicOrd [y | y<-xs, (fst y > fst x && snd y > snd x)]

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = [y | y<-xs , y<=x] ++ [x] ++ qSort [y | y<-xs , y>x]

-- 7.14
drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 ls = ls
drop' n (x:xs) = drop (n-1) xs

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n ls = (take n ls,drop' n ls)

-- 7.15
-- implement something like this take (-_) _ = error "Negative nummber"

-- 7.16
-- zip3 definition

--recursive one
zip3'r :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3'r a b c
  | null a || null b || null c = []
zip3'r (a:as) (b:bs) (c:cs) = [(a,b,c)] ++ zip3'r as bs cs

-- zip one
-- dunno how to add item to a tuple

-- 7.17
-- descending order and removing duplicates
qSortn :: [Int] -> [Int]
qSortn ls = reverse . qSort . nub $ ls

-- 7.18
-- sublist "ship" "Fish & Chips"
{-
"ship" "sh & Chips"
"hip" "h & Chips"
"ip" "ips"
"p" "ps"
"" "s"
-}

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList [] b = True
isSubList a [] = False
isSubList (x:xs) (y:ys)
  | x == y = True && isSubList xs ys
  | otherwise = isSubList (x:xs) ys

-- subsequence "Chip" "Fish & Chips"
isSubSequence :: Eq a => [a] -> [a] -> Bool
isSubSequence [] _ = True
isSubSequence _ [] = False
isSubSequence (x:xs) (y:ys)
  | x == y = subHelper xs ys (x:xs)
  | otherwise = isSubSequence (x:xs) ys
  where
    subHelper [] _ _ = True
    subHelper _ [] _ = False
    subHelper (x:xs) (y:ys) savedWord
      | x == y = True && subHelper xs ys savedWord
      | otherwise = isSubSequence savedWord (y:ys)


whitespace = ['\t','\n',' ']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
  | elem x whitespace = []
  | otherwise = x : getWord xs

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
  | elem x whitespace = (x:xs)
  | otherwise = dropWord xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
  | elem x whitespace = dropSpace xs
  | otherwise = (x:xs)

type Word = String

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

type Line = [Word]

getLine :: Int -> [Word] -> Line
getLine len [] = []
getLine len (w:ws)
  | length w <= len = w : restOfLine
  | otherwise = []
  where
    newlen = len - (length w + 1)
    restOfLine = getLine newlen ws

-- 7.19
dropLine :: Int -> [Word] -> Line
dropLine len [] = []
dropLine len (w:ws)
  | len >= length w = dropLine newlen ws
  | otherwise = w:ws
  where
    newlen = len - (length w +1)

lineLen = 17

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws
  = getLine lineLen ws
    : splitLines (dropLine lineLen ws)

fill :: String -> [Line]
fill = splitLines . splitWords

-- 7.20
joinLine :: Line -> String
joinLine [] = []
joinLine (w:ws)
  | null ws = w ++ joinLine ws
  | otherwise = w ++ " " ++ joinLine ws

-- 7.21
joinLines :: [Line] -> String
joinLines = concat . map (\n -> n ++ "\n") . map joinLine

{-putStr $ joinLines $ splitLines $ words "Errors are red, my screen is blue, i think i deleted system32"-}

-- 7.22 skipped
-- think it trash

-- 7.23
-- skipped too bc i don't know how it should looks like

-- 7.24
-- take a string and produce amount of (characters,words,lines)
wc :: String -> (Int,Int,Int)
wc ws = (length ws, length $ splitWords ws, length $ filter (=='\n') ws)

-- 7.25
-- filterPunc -> toLower -> isPalindrome
punctuation = " ,.'!?"

isPalin :: String -> Bool
isPalin = isPalindrome . map toLower . filter (\n -> not $ elem n punctuation)
  where
    isPalindrome a = a == reverse a

-- 7.26
{-
[How much  is that?] []
[How ] [uch  is that?] [m]
[How ] [ch  is that?] [mu]
[How ] [h  is that?] [muc]
[How ] [is that?] [much]
[How ] [is that?] [much ]
[How ] [tall] [ is that?]
-}

-- bogged  subst "hate" "love" "Hello, hatiko I hate you"
subst :: String -> String -> String -> String
subst _ _ [] = []
subst [] _ st = st
subst oldSub newSub string = subHelp oldSub newSub string oldSub []
  where
    --subHelp oldSub newSub nextStr savedSub prevStr
    subHelp [] newSub nextStr _ prevStr = prevStr ++ newSub ++ nextStr
    subHelp _ _ [] _ prevStr = prevStr
    subHelp (x:xs) newSub (y:ys) savedSub prevStr
      | x == y = subHelp xs newSub ys savedSub prevStr
      | x /= y = subHelp savedSub newSub ys savedSub (prevStr ++ [y])
