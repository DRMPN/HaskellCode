-- :set +s

import Test.QuickCheck (quickCheck)

-------------------------------------------------------------

-- nullary as they take no arguments
data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter
  deriving (Eq, Ord, Enum, Show, Read) -- added later

weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold
-------------------------------------------------------------

-- binary bc it takes two elements
data People = Person Name Age
type Name = String
type Age = Int
--unary constructors
--data Age = Years Int

showPerson :: People -> String
showPerson (Person st n) = st ++ " -- " ++ show n
-------------------------------------------------------------

data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float
  deriving (Eq, Ord, Show, Read) -- added later

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w
area (Triangle a b c) = sqrt $ p * (p-a) * (p-b) * (p-c)
  where p = perimeter (Triangle a b c) / 2

{-
Circle :: Float -> Shape
General definition of that is :
Con1 :: ti1 -> Typename

  The types can be recursive; we can use the type we are defining, Typename, as (part of) any of the types tij. This gives us lists, trees and many other data structures.
The Typename can be followed by one or more type variables which may be used on the right-hand side, making the definition polymorphic.

Contrasting type and data definitions:
  A synonym given by type is simply a shorthand, and so synonym type can always be expanded out, and thefore removed from the program. On the other hand, a data definition creates a new type. Because sumonyms are simply shorthand, a synonym definition cannot be recursive.
-}


-- 14.1
-- Redefine the function weather so that a guard or and if is used rather than pattern matching. Which of the definitions is preferable in your opinion
weather' :: Season -> Temp
weather' s
  | s == Summer = Hot
  | otherwise = Cold
-- I prefer that variant bc it looks better (and more readable bc it's onle a type definition and the funciton itself)

-- 14.2
-- Define the type of months as a Haskell algebraic type.
-- Give a function which takes a month to its approproate season.
data Months = Month Name Int deriving (Eq,Ord)
toSeason :: Months -> Season
toSeason (Month _ m)
  | m `elem` [9,10,11] = Autumn
  | m `elem` [3,4,5] = Spring
  | m `elem` [6,7,8] = Summer
  | otherwise = Winter

-- 14.3
-- What would be the weather function for New Zeland and for Brazil
nzWeather :: Season -> Temp
nzWeather s
  | s == Winter || s == Autumn = Hot
  | otherwise = Cold

brWeather :: Season -> Temp
brWeather _ = Hot

-- 14.4
-- Define a function to give the length of the perimeter of a geometrical shape, of type Shape.
-- What is the type of this functions?
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle h w) = 2 * h * w
perimeter (Triangle a b c) = a + b + c

-- 14.5
-- Add an extra constructor to Shape for triangles.
-- Extend the functions isRound area and perometer to include triangles.

-- 14.6
-- Define a funciton which decides whether a Shape is regular
isRegular :: Shape -> Bool
isRegular (Circle _) = True
isRegular (Rectangle h w)
  | h == w = True
  | otherwise = False
isRegular (Triangle a b c)
  | (a == b) && (b == c) = True
  | otherwise = False

-- 14.7
-- Investigate the derived definitions for Temp and Shape: what form do the orderings and the show fucntions take?
{-
Winter > Summer It means that constructor describes elements in increasing order
with show class elements are able to be printed
Eq means that elements of the data can be compared and etc
-}

-- 14.8
-- Define == over Shape so that all circles of negative radius are equated.
-- How would you treat rectangles with negative sides?
{-
instance Eq Shape where
  (==) (Circle r1) (Circle r2)
    | r1 == r2 = True
    | (r1 < 0) && (r2 <0) = True
    | otherwise = False
-}
-- If rectangle has negative side then it treats like nothing, it doesn't exist

-- 14.9
-- Modify the original definition of Shape to Contain the centre of each object
data NewShape = NewCircle (Float) (Float,Float) |
  NewRectangle (Float, Float) (Float,Float) |
  NewTriangle (Float, Float, Float) (Float,Float)
  deriving (Eq, Ord, Show, Read) -- added later

-- 14.10
-- Define a function which moves a shape by the two offsets
move :: Float -> Float -> NewShape -> NewShape
move x y (NewCircle rad (x0,y0)) = NewCircle rad (x0+x,y0+y)
move x y (NewRectangle sides (x0,y0)) = NewRectangle sides (x0+x,y0+y)
move x y (NewTriangle sides (x0,y0)) = NewTriangle sides (x0+x,y0+y)

-- 14.11
-- Define a function to test whether to NewShapes overlap

-- assume first shape is nearest to the x=0
-- assume we calculate only x overlap
isOverlapNS :: NewShape -> NewShape -> Bool
isOverlapNS newrec1 newrec2
  | fst coord1 + snd coord2 <= w1 + w2 = True
  | fst coord1 + snd coord2 > w1 + w2 = False
  | otherwise = error "Do something!"
  where
    coord1 = findWide newrec1
    coord2 = findWide newrec2
    w1 = fst coord1 + snd coord1
    w2 = fst coord2 + snd coord2
    findWide (NewRectangle (h, w) (x,y)) = (y-w/2,y+w/2)

-- 14.12
-- How would  you implement the type of 'strings or numbers' used as a part of an addresss?
-- Write a function which gives the textual form of one of these objects.
-- Give a definition of a type of names and adresses using the type you have defined.

data HouseAddress = HouseName String | HouseNumber Float
  deriving (Show, Read, Eq)

toTextForm :: HouseAddress -> String
toTextForm (HouseName a) = a
toTextForm (HouseNumber a) = show a

-- 14.13
-- Reimplement the library database of Section 5.6 to use an algebraic type like People rather than a pair.
{-
data Database = People (PersonName,PersonLoan) | Books (BookName) (WhoRented)
type PersonName = String
type PersonLoan = String
type BookName = String
type WhoRented = String
-}

-- 14.14
-- The library database is to be extended

{-
Probably, it's worth to reimplement database using record syntax like this:
data LibCatalog a b c d = LibItem {lib_author :: a,
                                   lib_title :: b,
                                   lib_type :: c,
                                   lib_period :: d}
-}

data LibDatabase = LibReaders | LibCatalog

data LibReaders = LibPerson (Lib_fullname,
                             (Lib_author, Lib_title, Lib_period) ) deriving Show

data LibCatalog = LibItem ( (Lib_author, Lib_title) ,
                            (Lib_type, Lib_period) ) deriving Show
type Lib_fullname = String
type Lib_author = String
type Lib_title = String
type Lib_type = String
type Lib_period = Int

-- Define functions:
-- Find all items on loan to a given person.
findPersonLoanItem db fullname = foldr (\(LibPerson (name, item)) acc -> if name == fullname
                                                                         then acc ++ [item]
                                                                         else acc) [] db
