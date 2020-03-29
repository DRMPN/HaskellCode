module Exercise10 where

import Prelude hiding (Word)
import Data.Char (toLower)
import Data.List (sort)

import Exercise2

import Pictures
-- :set +s

-- 10.1
-- redefine printBill so that composition is used
-- chapter 6 use >.> operator

infixl >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

-- code is already using composition so skipped
-- printBill = putStr >.> produceBill
-- or
-- printBill = putStr >.> makeBill >.> formatBill

-- 10.2
{-
explain behavior of
(id . f)        (f . id)        id f

1. first use function f then identify the result of f
    so its like (a -> b) => b -> b
2. first identify the input and then use function f
    so like (a -> a) => (a -> b)
3. will be the same transformation as funciton f
-}

-- 10.3
-- define a funciton to compose a list of funcitons into a single function
composeList :: Foldable t => t (a -> a) -> a -> a
-- ezy type is [(a -> a)] -> (a -> a)
composeList a = foldr (\n acc -> acc . n) id a

-- it has a super polymorphic type of Foldable because it takes a list of functions and produces a function with type of a -> a
-- the effect on an empty list is id function
-- works pretty much like +0 or like *1 or like /1

-- 10.4
-- give calculation of
{-
iter 3 double 1
iter n f = foldr (.) id (replicate n f)
iter 2 double (double 1)
..
iter 0 double (double . double . double $ 1) 1
16

(comp succ (*)) 3 4
succ 3 -> 4
succ 4 -> 5
4 * 5 -> 20

comp2 sq add 3 4
sq 3 -> 9
sq 4 -> 16
add 9 16 -> 25
-}

iter :: Int -> (b -> b) -> b -> b
iter n f = foldr (.) id (replicate n f)

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y))

-- 10.5
-- what is the type and effect
-- \n -> iter n succ
-- Enum a => Int -> a -> a
-- effect will be the same as iter

-- 10.6
-- function a -> b -> c
-- write down definiiton b -> a -> c
-- \ f a b -> f b a

-- 10.7
-- give a definition of the function :
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' = \ f a b -> f b a

-- 10.8
-- use lambda expression with function not and elem to describe a function
-- Char -> Bool
-- \ n -> not $ elem n " \t\n"

-- 10.9
-- define a funciton total:
-- don't know what that function should do but it's my improvisation
total :: (Int -> Int) -> (Int -> Int)
total f n = sum $ map (\n -> f n) [0..n]

-- 10.10
-- takes f as argument and returns its deriviate f' as the result
-- slope :: (Float -> Float) -> (Float -> Float)
-- !!! TODO (just don't know what is slope and how to deal with it)

-- 10.11
-- takes function f as argument and returns the two arguments function which gives the area under its graph
-- integrate :: (Float -> Float) -> (Float -> Float -> Float)
-- !!! TODO (for detail see 10.10)

-- (op x) y -> y op x
-- (x op) y -> x op y

-- 10.12
-- use partial application to define comp2 and total
-- a what???

-- 10.13
-- find sec1 and sec2 so
-- map sec1 . filter sec2
-- has the same effect
-- filter (>0) . map (+1)

-- sec1 is (+1), sec2 is (>(-1))

-- 10.14
-- define a chessBoard so that n is a picture of an n by n chess board
chessBoard :: Int -> Picture
chessBoard a = chessGen a a "black" []

-- produces either square of black and white or squre of white and blck
chessGen :: Int -> Int -> String -> Picture -> Picture
chessGen num a color pic
  | a == 0 = pic
  | color == "black" = chessGen num (a-1) "white" (pic `above` lineGen num "black" myBlack)
  | otherwise = chessGen num (a-1) "black" (pic `above` lineGen num "white" myWhite)

--produces a line of black and white or a line of white and black
lineGen :: Int -> String -> Picture -> Picture
lineGen num color list
  | num == 1 = list
  -- 1 bc we pass pic to func
  | color == "black" = lineGen (num-1) "white" (sideBySide list myWhite)
  | otherwise = lineGen (num-1) "black" (sideBySide list myBlack)

-- 10.15
-- how you would you implement invertColour superimpose printPicture if Picture
-- is defined like [[Bool]]
type BoolPic = [[Bool]]

invertBoolColour :: BoolPic -> BoolPic
invertBoolColour = map invertBoolLine
  where invertBoolLine = map (\n -> if n == True then False else True)

superimposeBool :: BoolPic -> BoolPic -> BoolPic
superimposeBool = zipWith . zipWith $ imposeBoolLine
  where imposeBoolLine topCh botCh = if topCh == False && botCh == False then False else True

-- printBoolPic - I did it somewhere, so i'm lazy to write it again

-- 10.16
-- makes a picture with black dots
-- consumes width higth and a list of positions of black dots
makePicture :: Int -> Int -> [(Int,Int)] -> Picture
makePicture w h list = foldr (\pos pic -> addDot pos pic) picture list
  where picture = makePic w h

-- makePic 5 5
makePic :: Int -> Int -> Picture
makePic w h = replicate h . replicate w $ '.'

-- changeNthElement 3 (changeNthElement 2 (\n -> '#')) (makePic 5 5)
addDot :: (Int,Int) -> Picture -> Picture
addDot (x,y) pic = changeNthElement x (changeNthElement y (\n -> '#')) pic

-- helper code from overflow
-- nothing special just splitAt function and pattern matching
changeNthElement :: Int -> (a -> a) -> [a] -> [a]
changeNthElement id func list
    | id < 0   = list
    | otherwise = case splitAt id list of
                    (front, element:back) -> front ++ func element : back
                    _ -> list

-- 10.17
-- reverse effect of makePircure
-- read top then down to top
testpic = ["....",".##.","...."]

-- main function
pictureToRep :: Picture -> (Int, Int , [(Int,Int)])
pictureToRep a = (width,height,dotsPos a)
  where width = head . map length $ a
        height = length a
        dotsPos = combinedDots . picDotsX

-- it's probably worth it to encapsulate 4 functions in 2

combinedDots :: [[Int]] -> [(Int,Int)]
combinedDots = findAndCombineDots 0 []

-- findAndCombineDots 0 [] $ picDotsX testpic
-- findAndCombineDots 0 [] [[],[1,2],[]]
findAndCombineDots :: Int -> [(Int,Int)] -> [[Int]] -> [(Int,Int)]
findAndCombineDots y list xCoords
  | null xCoords = list
  | null firstLine = findAndCombineDots (y+1) list restX
  | otherwise = findAndCombineDots (y+1) (list ++ coordinates) restX
    where firstLine = head xCoords
          restX = tail xCoords
          coordinates = map (\x -> (y,x)) firstLine

picDotsX :: Picture -> [[Int]]
picDotsX pic = map (findDotsX 0 []) pic

-- it was LITERALLY 5Head decision
-- map (findDot 0 []) ["....",".##.","...."]
findDotsX :: Int -> [Int] -> String -> [Int]
findDotsX x list line
  | null line = list
  | first == '#' = findDotsX (x+1) (list ++ [x]) rest
  | otherwise = findDotsX (x+1) list rest
    where first = head line
          rest = tail line

-- 10.18
{-
type Rep = (Int, Int, [(Int, Int)])
discuss how you would define functions over rep rotate, reflect and superimpose pictures
discuss the adventages and disadventages of this representation in comparison with original
-}

{-
Okay, I see 2 ways of doing this.
First one is just to use old functions with type Picture. So, firstly you made a pic with type Rep and then use already exsisted functions to rotate them, superimpose and so on.
Second one is to implement new functions that will work with new Rep type. It won't be so fast as it would be with first method, it's complicated, and boring? On the other hand, you will get a boost in work speed, less memory usage and a feeling of satisfactory :)

Main problem with original representation is that you have to type pictures by yourself! It's like pixel drawing. Problem with new method - is there any problem? You can easily create a blank picture and then just guess needed dots.
-}
-- i'd done

-- 10.19
-- redo the exercises of section 6.2 with positioned pictures
-- TODO i'm lazy, but i'll do that (probably)

{-
PARTIALLY APPLIED FUNCTIONS
if we calll a funciton with too few parameters
max 4

CURRIED FUNCTIONS
putting space between two things is simply function application

so function max can be written like this
max 4 5 or (max 4) 5 they are the same
type of max function is
max :: Ord a => a -> a -> a
or
max :: Ord a => a -> (a -> a)      - this could be read like this: max takes an a and returns a function that takes an a and returns an a
-}

-- #######################################
-- # something like wordcounter on lines #
-- #######################################
type Doc = String
type Line = String
type Word = String

makeIndex :: Doc -> [ ([Int], Word) ]
makeIndex =
  shorten .        -- [([Int], Word)] -> [([Int], Word)]
  amalgamate .     -- [([Int], Word)] -> [([Int], Word)]
  makeLists .      -- [(Int, Word)]   -> [([Int], Word)]
  sortLs .         -- [(Int, Word)]   -> [(Int, Word)]
  allNumWords .    -- [(Int, Line)]   -> [(Int, Word)]
  numLines .       -- [Line]          -> [(Int, Line)]
  lines            -- Doc             -> [Line]

numLines :: [Line] -> [(Int, Line)]
numLines lines = zip [1 .. length lines] lines

numWords :: (Int, Line) -> [(Int, Word)]
numWords (number, line) = map (\word -> (number, word)) (splitWords line)
  where splitWords = words -- TODO i was lazy to implemet it by myself , so later define with whitespace p.s for more detail look in Exercise7

whitespace :: String
whitespace = " \n\t;:.,\'\"!?()-"

allNumWords :: [(Int, Line)] -> [(Int, Word)]
allNumWords = concat . map numWords

-- quicksort alforithm is used
sortLs :: [(Int, Word)] -> [(Int, Word)]
sortLs [] = []
sortLs (p:ps) = sortLs smaller ++ [p] ++ sortLs larger
  where
    smaller = [q | q <- ps, orderPair q p]
    larger = [q | q <- ps, orderPair p q]

orderPair :: (Int, Word) -> (Int, Word) -> Bool
orderPair (n1, w1) (n2, w2) = w1 < w2 || (w1 == w2 && n1 < n2)

makeLists :: [(Int, Word)] -> [([Int], Word)]
makeLists = map mklis
  where mklis (n , st) = ([n], st)

amalgamate :: [([Int], Word)] -> [([Int],Word)]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1,w1):(l2,w2):rest)
  | w1 /= w2 = (l1, w1) : amalgamate ((l2, w2):rest)
  | otherwise = amalgamate ((l1++l2,w1):rest)

shorten :: [([Int], Word)] -> [([Int],Word)]
shorten = filter sizer
  where sizer (nl, wd) = length wd > 3

-- 10.20
-- define funciton lines using takeWhile and dropWhile
-- do not give empty word "cat\n\ndog" and "fish\n"
myLines :: String -> [String]
myLines [] = []
myLines (x:xs)
  | x == '\n' = myLines xs
  | otherwise = first : (myLines rest)
    where
      first = takeWhile (/= '\n') (x:xs)
      rest = dropWhile (/= '\n') (x:xs)

-- 10.21
-- use lambda expressions to replace the local definitoins of makelist and shorten
-- then use comprehensions
{-
makeLists = map (\(n,st) -> ([n],st))
makeLists list = [([n], st) | (n, st) <- list ]

shorten = filter (\(_,wd) -> length wd > 3)
shorten list = [(nl,st) | (nl, st) <- list, length st > 3]
-}

-- 10.22
-- makeIndex "\n\ncathedral\n\ncathedral\ncathedral\ncathedral\n\ncathedral\ncathedral\n"
-- instead of input like cathedral 3,5,6,7,9,10 m
-- make cathedral 3,5-7,9-10
makeNewIndex :: Doc -> [([[Int]], Word)]
makeNewIndex = changeTuples . makeIndex

-- changes to number ranges in the list of tuples
changeTuples :: [([Int], Word)] -> [([[Int]], Word)]
changeTuples = map (\(lof, word) -> (makeRange lof, word))
  where
    -- makeRange    [3,5,6,7,9,10]
    -- produces     [[3],[5,6,7],[9,10]]
    makeRange xs = numberRanges (head xs - 1) [] [] xs -- TODO reforge this bycicle
    -- main function
    numberRanges _ newRange newList [] = newList ++ [newRange]
    numberRanges lastNum newRange newList (x:xs)
      | lastNum + 1 == x = numberRanges x (newRange ++ [x]) newList xs
      | otherwise = numberRanges x (x:[]) (newList ++ [newRange]) xs

-- combines two lists in one
-- myAppend [1,2,3] [4,5,6] - > [1,2,3,4,5,6]
myAppend :: [a] -> [a] -> [a]
myAppend first second = foldr (:) second first

-- 10.23
-- redefine sortLs so that duplicate copies of an item are not removed
sortLs' [] = []
sortLs' (p:ps) = sortLs' smaller ++ [p] ++ sortLs' bigger
  where smaller = [q | q <- ps, orderPair' q p]
        bigger = [q | q <- ps, orderPair' p q]
        orderPair' (n1, w1) (n2, w2) = w1 < w2 || (w1 == w2 && n1 < n2)
-- !!! TODO i literally don't know you can't change something you have to completly rewrite this

-- 10.24
-- how could the functions getUntil and dropUnitl be used in amalgamate
-- getUntil w1 /= w2 ++ dropWhile w1 /= w2

-- 10.25
-- explain how the function sizer can be defined as composition of built in functions
-- sizer = length (>3) . snd

-- 10.26
-- how is the definition of amalgamate incorrect?
-- the function takes (firstTuple:secondTuple:rest) therefore if the argument consist of 1 tuple, function will get error bc it's not pattern matching argument like this - firstArgument:[]

-- 10.27
-- printIndex [([2],"Battey"),([1,2,3],"Cathedral"),([1,2],"Doggerel")] ->
--        Battey         2
--        Cathedral      1, 2, 3
--        Doggerel       1, 2
printIndex :: [([Int], Word)] -> IO ()
printIndex = putStr . showIndex

showIndex :: [([Int], Word)] -> String --  or Doc
showIndex = concat . map makeLine
  where makeLine (nums, words) = pushLeft words ++ convertNumbers nums ++ "\n"

-- use foldr with numbers (\n acc -> acc ++ ", " ++ [n]) -- check first number
convertNumbers :: [Int] -> String
convertNumbers = addStuff ""
  where
    -- empty list case?
    addStuff strNums [n] = strNums ++ show n
    addStuff strNums (n:ns) = addStuff ((++)strNums . (++) (show n) $ ", ") ns

maxwordlength = 20
pushLeft :: Word -> String
pushLeft a = a ++ (replicate (maxwordlength - (length a)) ' ')

-- 10.28
-- modify the program so that words of less than four letters are removed as a part of the definition of allNumWords
makeIndexMod1 :: Doc -> [ ([Int], Word) ]
makeIndexMod1 =
  shorten .        -- [([Int], Word)] -> [([Int], Word)]
  amalgamate .     -- [([Int], Word)] -> [([Int], Word)]
  makeLists .      -- [(Int, Word)]   -> [([Int], Word)]
  sortLs .         -- [(Int, Word)]   -> [(Int, Word)]
  allNumWords' .   -- [(Int, Line)]   -> [(Int, Word)]      - modification
  numLines .       -- [Line]          -> [(Int, Line)]
  lines            -- Doc             -> [Line]

-- easier to filter words that are less then four on the previus step (numWords)
allNumWords' :: [(Int, Line)] -> [(Int, Word)]
allNumWords' = concat . map numWords'

numWords' :: (Int, Line) -> [(Int, Word)]
numWords' (number, line) =
  map (\word -> (number, word)) coolWords
  where
    coolWords = filter (\w -> length w > 4) . words $ line

-- 10.29
-- modify the makeIndex funciton so that instead of returning the list of line numbers the function returns the total number of times that the word occurs
-- make sure that multiple occurrences of a word in a single line are counted
-- 1) modify the program as little as is necessary - return the length of a list rather than the list itself
--    note : modify it before sortLs
-- 2) take the program structure as a guide and write a simpler program which calculates the number of occurrences directly

-- I method
makeIndexMod2 :: Doc -> [(Int, Word)]
makeIndexMod2 =
  sortLs .                 -- [(Int, Word)]   -> [(Int, Word)]
  uniteAllWordsStarter .   -- [(Int, Word)]   -> [(Int, Word)]
  allNumWords .            -- [(Int, Line)]   -> [(Int, Word)]
  numLines .               -- [Line]          -> [(Int, Line)]
  lines                    -- Doc             -> [Line]

uniteAllWordsStarter :: [(Int,Word)] -> [(Int,Word)]
uniteAllWordsStarter = uniteAllWords []
  where
    -- lot low
    uniteAllWords lot [] = lot
    uniteAllWords lot (x:xs) = countWords lot sameWords restWords
      where sameWords
              | null xs = [x]
              | otherwise = filter (\(n,w) -> w == snd x) (x:xs)
            restWords = filter (\(n,w) -> w /= snd x) (x:xs)
  -- lot sameWords restWords
    countWords lot [] restWords = uniteAllWords lot restWords
    countWords lot sameWords restWords =
      uniteAllWords (lot ++ [(length sameWords, word)]) restWords
      where word = map (toLower) . snd . head $ sameWords


-- II method
wordCounter :: Doc -> [(Int,Word)]
wordCounter = startMakeLOT . wordSorting

-- sort words in alphabetical order and lowercase them
wordSorting :: String -> [Word]
wordSorting = sort . map (map toLower) . myWords

-- produces a list of clean words (without punctuation marks)
myWords :: String -> [Word]
myWords [] = []
myWords (x:xs)
  | elem x whitespace = myWords xs
  | otherwise = first : (myWords rest)
    where
      first = takeWhile notPM (x:xs)
      rest = dropWhile notPM (x:xs)
      -- old (\n -> not $ elem n whitespace)
      notPM = not . flip elem whitespace

-- ################ - Here I used an expereince from UBCx course to make that recurion
--                         probably bad, but that how I remember it

-- takes    : list of words
-- produces : list of tuples
startMakeLOT :: [Word] -> [(Int,Word)]
startMakeLOT = makeLOT []
  where
  -- function takes : lot low
    makeLOT lot [] = lot
    makeLOT lot (x:xs) = filterList x xs (0,x) lot
  -- function takes : word low tuple lot
    filterList word [] (n,w) lot = makeLOT (lot ++ [(n+1, w)]) []
    filterList word (x:xs) (n,w) lot
      | word == x = filterList word xs (n+1, w) lot
      | otherwise = makeLOT (lot ++ [(n+1, w)]) (x:xs)

-- 10.30
-- modify the program so that capitalized words like "Dog" are indexed under their uncapitalized equivalents "dog"
-- this does not work well for proper names like "Amelia" - what could you do about that?

makeIndexMod3 :: Doc -> [ ([Int], Word) ]
makeIndexMod3 =
  shorten .        -- [([Int], Word)] -> [([Int], Word)]
  amalgamate .     -- [([Int], Word)] -> [([Int], Word)]
  makeLists .      -- [(Int, Word)]   -> [([Int], Word)]
  sortLs .         -- [(Int, Word)]   -> [(Int, Word)]
  allLowerWords .         -- [(Int, Word)]   -> [(Int, Word)]
  allNumWords .    -- [(Int, Line)]   -> [(Int, Word)]
  numLines .       -- [Line]          -> [(Int, Line)]
  lines            -- Doc             -> [Line]

listOfNames = ["Amelia"]

-- lowercases all words before sorting them and filters names
allLowerWords :: [(Int,Word)] -> [(Int, Word)]
allLowerWords = map lowerTuple
  where
    lowerTuple (num,word)
      | elem word listOfNames = (num, word)
      | otherwise = (num, map toLower word)

