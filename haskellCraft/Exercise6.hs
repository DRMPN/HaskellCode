import Pictures hiding (superimpose, printPicture)
import Data.List (transpose, nub, delete, groupBy, sort)

-- 6.1
superimposeChar :: Char -> Char -> Char
superimposeChar a b
  | (a == '.') && (b == '.') = '.'
  | otherwise = '#'

-- 6.2
superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine = zipWith superimposeChar

-- 6.3
superimpose :: Picture -> Picture -> Picture
superimpose = zipWith superimposeLine

-- 6.4
printPicture :: Picture -> IO()
printPicture = putStr . concat . map (++"\n")
-- [".##." , ".#.#" , ".###" , "####"]

-- 6.5
type BoolPic = [[Bool]]
-- black - [[True]]
-- white - [[False]]

makeBoolPic :: Bool -> BoolPic
makeBoolPic b =
  let makeBoolLine b = replicate 5 b
  in replicate 8 (makeBoolLine b)

blackBool :: BoolPic
blackBool = makeBoolPic True

whiteBool :: BoolPic
whiteBool = makeBoolPic False

-- different length of bool values
-- could create a custom function to make white and black the same
-- but if that values was near then the length of the line would be different
printBoolPic :: BoolPic -> IO()
printBoolPic = putStr . concat . map (++"\n") . map concat . map (map show)

-- 6.6
-- [".." , "##"] in to ["#." , "#."]
-- import Data.List (transpose)
-- rotate90 = transpose . reverse

rotate90 :: Picture -> Picture
rotate90 = transpose . reverse

-- 6.7
-- !!! reforge

rotateAnti90 :: Picture -> Picture
rotateAnti90 = rotate90 . rotate90 . rotate90

-- 6.8

--  concat (map (replicate 2) (scale ["#.#","..#"] 2))

scale :: Picture -> Int -> Picture
scale pic n =
  let
    -- helperScale :: Int -> [Char] -> [Char]
    hlpScl1 n xs = concat [replicate n x | x <- xs]
    -- scale :: Picture -> Int -> Picture
    hlpScl2 pic n = map (hlpScl1 n) pic
  in concat . map (replicate n) $ (hlpScl2 pic n)

-- 6.9

type Position = (Int, Int)

type Image = (Picture, Position)

makeImage :: Picture -> Position -> Image
makeImage pic pos = (pic, pos)

-- 6.10

changePosition :: Image -> Position -> Image
changePosition (pic, pos) npos = (pic, npos)

-- 6.11

moveImage :: Image -> Int -> Int -> Image
moveImage (pic,(a,b)) x y = (pic , (a + x, b + y))

-- 6.12

printImage :: Image -> IO()
printImage (pic,_) = printPicture pic

-- 6.13
-- analogy flipH flipV rotate rotate90

nflipH :: Image -> Image
nflipH (pic, pos) = (flipH pic, pos)

nflipV :: Image -> Image
nflipV (pic, pos) = (flipV pic, pos)

nrotate :: Image -> Image
nrotate (pic, pos) = (rotate pic, pos)

nrotate90 :: Image -> Image
nrotate90 (pic, pos) = (rotate90 pic, pos)

-- 6.14

gflipH :: Image -> Image
gflipH (pic, (x,y)) = (flipH pic, (x, y - length pic))

gflipV :: Image -> Image
gflipV (pic, (x,y)) = (flipV pic, ((-) x $ length . head $ pic, y))

grotate :: Image -> Image
grotate (pic, (x,y)) = (rotate pic , ((-) x $ length . head $ pic , y - length pic))

grotate90 :: Image -> Image
grotate90 (pic, pos) = (rotate90 pic, pos)

-- 6.15
-- 6.16
-- 6.17
-- !!! todo but this todo is shit

makeCellV :: Picture
makeCellV = replicate 12 $ replicate 6 '.'

makeCellH :: Picture
makeCellH = replicate 4 $ replicate 18 '.'

padding :: Picture -> Picture
padding a = horse

-- let firstPic = above (sideBySide makeCellV horse) $ makeCellH

-- printPicture . superimpose firstPic $ rotate firstPic

-- 6.18 && 6.19

maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs x y z =
  let maxOfThree a b c
        | a > b && b > c = a
        | b > c = b
        | otherwise = c
      maxThree = maxOfThree x y z
  in (maxThree, length $ filter (==maxThree) [x,y,z])

{-
##############################################################
##################### SUPERMARKET BILLING ####################
##############################################################
-}

-- REFORGE :
-- 1) dotString to separate function and redefine
-- 2) remove "Unknown Item" from code

-- ######### Constants and Definitions #########
type Name = String
type Price = Int
type BarCode = Int

type TillType = [BarCode]
type BillType = [(Name,Price)]

type Database = [ (BarCode,Name,Price) ]

lineLength :: Int
lineLength = 30

codeIndex :: Database
codeIndex = [
  (4719, "Fish Fingers", 121),
  (5643, "Napppies", 1010),
  (3814, "Orange Jelly", 56),
  (1111, "Hula Hoops", 21),
  (1112, "Hula Hoops (Giant)", 133),
  (1234, "Dry Sherry, ilt", 540)]

-- 6.28
-- list of bar codes to list of name + price
makeBill :: TillType -> BillType
makeBill codes = map lookUp codes

-- 6.25
-- list of pairs to list of formatted bill
formatBill :: BillType -> String
formatBill a =
  let produceDiscount = formatDiscount . makeDiscount
      produceTotal a = formatTotal $ makeTotal a - makeDiscount a
  in formatLines a ++ produceDiscount a ++ produceTotal a

-- putStr $ produceBill [1234,4719,3814,1112,1113,1234]
-- bill that ready to be printed
produceBill :: TillType -> String
produceBill = formatBill . makeBill

-- 6.20
-- make a price of number
-- i.e from 1212 to 12.12
formatPence :: Price -> String
formatPence n =
  let helperDiv a = show $ div a 100
      helperMod a
        | length num < 2 = "0" ++ num
        | otherwise = num
        where num = show $ mod a 100
  in helperDiv n ++ "." ++ helperMod n

-- 6.21
-- make a bill line of name and price within lineLength
-- i.e from "kek" 12 to "kek...12\n"
-- !1! REFORGE
formatLine :: (Name,Price) -> String
formatLine (nm, pr) =
  let price = formatPence pr
      makeLine n p = n ++ dotString n p ++ p
      dotString n p = replicate ((-) lineLength $ length . (++) n $ p) '.'
  in makeLine nm price ++ "\n"

-- 6.22
-- from list of name/price to line of name/price
-- in addition it removes unkown items
-- !2! REFORGE
formatLines :: BillType -> String
formatLines = concat . map formatLine . filter (\n -> fst n /= "Unknown Item")
-- formatLines = concat . map formatLine

-- 6.23
-- make a full price of a list of name/price
makeTotal :: BillType -> Price
makeTotal = sum . map snd

-- 6.24
-- make a bill line of total price within lineLength
-- !1! REFORGE
formatTotal :: Price -> String
formatTotal price =
  let makeLine n p = n ++ dotString n p ++ p
      dotString n p = replicate ((-) lineLength $ length . (++) n $ p) '.'
  in (++) "\n" $ makeLine "Total" $ formatPence price
--in (++) "\n" $ makeLine "Total" $ show price

-- 6.26
-- find item that matches given code and produce either unknown item or real item
look :: Database -> BarCode -> (Name,Price)
look db code =
  let findCode db
        | null db = ("Unknown Item", 0)
        | otherwise = extractCode db
      extractCode [(c,n,p)] = (n,p)
  in findCode $ filter (\(c,n,p) -> c==code) db

-- 6.27
-- function that ready to work with constant assortiment
lookUp :: BarCode -> (Name, Price)
lookUp = look codeIndex

-- 6.29
-- calculate discount price from list of items
makeDiscount :: BillType -> Price
makeDiscount a =
  let findSherry = length . filter (\(n,_) -> n == "Dry Sherry, ilt")
  in (*) 100 $ findSherry a `div` 2

-- !1! REFORGE
-- make a bill line of discount
formatDiscount :: Price -> String
formatDiscount price =
  let makeLine n p = n ++ dotString n p ++ p
      dotString n p = replicate ((-) lineLength $ length . (++) n $ p) '.'
  in "\n" ++ (makeLine "Discount" $ formatPence price) ++ "\n"

-- 6.30
--  if     barcode is already exists
--  then   change name/price
--  else   add barcode/name/price
updateDatabase :: Database -> BarCode -> Name -> Price -> Database
updateDatabase db cd nm pr = (:) (cd,nm,pr) . filter (\(c,n,p) -> c /= cd) $ db

-- 6.31
-- done

-- 6.32
-- NEW PROJECT
-- bill analyzer

{-
TODO:
 1) remove Unknown Item from table
 2) sort table in descending order
 3) HARD analyze bills to see which pairs of items are bought together
-}

-- print table of items
printTotalSales :: TillType -> IO()
printTotalSales = putStr . sortListOfCodes

-- sort items into pairs
sortListOfCodes :: TillType -> String
sortListOfCodes a = makeLinesOfTable . groupBy (==) $ sort a

-- produce raw table
makeLinesOfTable :: [TillType] -> String
makeLinesOfTable = concat . map requestLineOfTable

-- request raw of table
requestLineOfTable :: TillType -> String
requestLineOfTable a =
 let takeName = fst . lookUp . head
 in makeLineOfTable (takeName a) (length a)

-- make row of table
makeLineOfTable :: Name -> Int -> String
makeLineOfTable name amount =
 let lineLength = 20
     makeLine n a = n ++ emptyString n a ++ a
     emptyString n a = replicate ((-) lineLength $ length . (++) n $ a) ' '
 in "\n" ++ (makeLine name $ show amount)
