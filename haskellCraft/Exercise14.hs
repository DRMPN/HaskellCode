-- :set +s
import Data.List (sort)
import Test.QuickCheck

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
data PeoplePrim = PersonPrim NamePrim AgePrim
type NamePrim = String
type AgePrim = Int
--unary constructors
--data Age = Years Int

showPersonPrim :: PeoplePrim -> String
showPersonPrim (PersonPrim st n) = st ++ " -- " ++ show n
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
{-
data LibDatabase a = LibReaders a | LibCatalog a

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
-}
--------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------

-- assume in the library only one copy of each book
data LibDatabase = LibDBPerson | LibDBCatalog -- TODO not sure about that construction

data LibDBPerson = NotLibPerson | LibPerson [LibReader]
data LibReader = Reader Name [Item]

data LibDBCatalog = NotLibItem | LibItem [Item]
data Item = NonAuthT ItemType Title Period | AuthT ItemType (Title, Author) Period
  deriving (Show)
data ItemType = Video | Book | CD deriving (Show, Eq)

type Title = String
type Author = String
type Period = Int

--------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------

book1, book2, cd1, cd2, vid1, vid2 :: Item
book1 = AuthT Book ("How to cook a tire.","John Tired") 30
book2 = AuthT Book ("How to read", "Abrahm Link") 21
cd1 = AuthT CD ("Photoset master", "Julia Gankoff") 7
cd2 = AuthT CD ("Don't do it yourself", "Mike Tyloon") 3
vid1 = NonAuthT Video "10000 degree knife vs luxury soap" 3
vid2 = NonAuthT Video "Pokecode: Code them all" 1

libcat1,libcat2 :: LibDBCatalog
libcat1 = LibItem [book1, cd1, vid1]
libcat2 = NotLibItem

per1,per2,per3 :: LibReader
per1 = Reader "Ilya Sokolov" [vid2]
per2 = Reader "Arthur Morgan" [book2,cd2]
per3 = Reader "Anna Shzkwar" []

libdbper1,libdbper2 :: LibDBPerson
libdbper1 = LibPerson [per1,per2,per3]
libdbper2 = NotLibPerson

--------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------

-- Find all items on loan to a given person.
findAllPerLoan :: LibReader -> [Item]
findAllPerLoan (Reader name loan) = loan

-- Find all books, CDs or videos on loan to a particular person
findPerLoan :: ItemType -> LibReader -> [Item]
findPerLoan it (Reader name loi) = findAllWithType it [] loi
  where
    findAllWithType _ list [] = list
    findAllWithType giventype list (x:xs)
      | giventype == listitemtype = findAllWithType giventype (list ++ [x]) xs
      | otherwise = findAllWithType giventype list xs
      where listitemtype = getType x
    getType (NonAuthT itemtype _ _) = itemtype
    getType (AuthT itemtype _ _) = itemtype

-- Find all items in the database due back on or before a particular day, and the same information  for any given person

-- assume that Period in database always updated with current data and could be negative if person have exiperd loan
-- assume items it catalog always with full period and it can't be change unless someone will take it from the library
--------------------------------------------------------------------
--------------------------------------------------------------------


data NTree = NilT | Node Int NTree NTree deriving (Show, Eq)

sumTree, depth :: NTree -> Int

sumTree NilT = 0
sumTree (Node n t1 t2) = n + sumTree t1 + sumTree t2

depth NilT = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

-- sumTree (Node 3 (Node 4 NilT NilT) NilT)
-- depth (Node 3 (Node 4 NilT NilT) NilT)

occurs :: NTree -> Int -> Int
occurs (Node n t1 t2) p
  | n == p = 1 + occurs t1 p + occurs t2 p
  | otherwise = occurs t1 p + occurs t2 p


data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr |
            Mult Expr Expr|
            Div Expr Expr
--Lit 2
--Add (Lit 2) (Lit )
--Add (Sub (Lit 3) (Lit 1)) (Lit 3)

evalExpr :: Expr -> Int
evalExpr (Lit n) = n
evalExpr (Add e1 e2) = (evalExpr e1) + (evalExpr e2)
evalExpr (Sub e1 e2) = (evalExpr e1) - (evalExpr e2)
evalExpr (Mult e1 e2) = (evalExpr e1) * (evalExpr e2)
evalExpr (Div e1 e2) = (evalExpr e1) `div` (evalExpr e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2)
  = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2)
  = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++ ")"
showExpr (Mult e1 e2)
  = "(" ++ showExpr e1 ++ "*" ++ showExpr e2 ++ ")"
showExpr (Div e1 e2)
  = "(" ++ showExpr e1 ++ "/" ++ showExpr e2 ++ ")"

countLit :: Expr -> Int
countLit (Lit n) = 1
countLit (Add e1 e2) = countLit e1 + countLit e2
countLit (Sub e1 e2) = countLit e1 + countLit e2
countLit (Mult e1 e2) = countLit e1 + countLit e2
countLit (Div e1 e2) = countLit e1 + countLit e2


data InfixExpr = InfixLit Int | InfixExpr :+: InfixExpr | InfixExpr :-: InfixExpr

evalInfExp :: InfixExpr -> Int
evalInfExp (InfixLit n) = n
evalInfExp (e1 :+: e2) = evalInfExp e1 + evalInfExp e2
evalInfExp (e1 :-: e2) = evalInfExp e1 - evalInfExp e2

-- eval1 $ (Lit1 2) :+: (Lit1 2)

showInfExp :: InfixExpr -> String
showInfExp (InfixLit n) = show n
showInfExp (e1 :+: e2) = "(" ++ showInfExp e1 ++ "+" ++ showInfExp e2 ++ ")"
showInfExp (e1 :-: e2) = "(" ++ showInfExp e1 ++ "-" ++ showInfExp e2 ++ ")"

countInfExp :: InfixExpr -> Int
countInfExp (InfixLit n) = 1
countInfExp (e1 :+: e2) = (countInfExp e1) + (countInfExp e2)
countInfExp (e1 :-: e2) = (countInfExp e1) + (countInfExp e2)


-- 14.15
-- Give calculations of
{-

eval (Lit 67) = 67
eval (Add (Sub (Lit 3) (Lit 1)) (Lit3 )) = (Sub (Lit 3) - (sub 1) ) + (Lit 3)
      = ( 3 - 1 ) + 3 = 5
show (Add (Lit 67) (Lit (-34))) = show (Lit 67) + (Lit (-34)) = "(67+-34)"

-}

-- 14.16
-- Define the functions which counts the number of operators in an expression
sizeExpr :: Expr -> Int
sizeExpr (Lit n) = 0
sizeExpr (Add e1 e2) = sizeExpr e1 + sizeExpr e2 + 1
sizeExpr (Sub e1 e2) = sizeExpr e1 + sizeExpr e2 + 1
sizeExpr (Mult e1 e2) = sizeExpr e1 + sizeExpr e2 + 1
sizeExpr (Div e1 e2) = sizeExpr e1 + sizeExpr e2 + 1

-- 14.17
-- Add the operations of multiplication and integer division to the type Expr
-- Redefine the functions eval,show and size to include these new cases

-- 14.18
-- Show how the functions eval, show and size are defined fo the new type

data ExprMod = LitMod Int | Op Ops ExprMod ExprMod
data Ops = AddMod | SubMod | MulMod | DivMod deriving (Eq, Show)

evalMod :: ExprMod -> Int
evalMod (LitMod n) = n
evalMod (Op ops e1 e2)
  | ops == AddMod = evalWith (+)
  | ops == SubMod = evalWith (-)
  | ops == MulMod = evalWith (*)
  | otherwise = evalWith (div)
  where
    evalWith n = n (evalMod e1) (evalMod e2)
-- evalMod (Op AddMod (LitMod 2) (LitMod 2))

showMod :: ExprMod -> String
showMod (LitMod n) = show n
showMod (Op ops e1 e2)
  | ops == AddMod = combineWith "+"
  | ops == SubMod = combineWith "-"
  | ops == MulMod = combineWith "*"
  | otherwise = combineWith "/"
    where
      combineWith operator = "(" ++ showMod e1 ++ operator ++ showMod e2 ++ ")"

sizeMod :: ExprMod -> Int
sizeMod (LitMod _) = 0
sizeMod (Op _ e1 e2) = 1 + (sizeMod e1) + (sizeMod e2)

-- 14.19
{-
sumTree (Node 3 (Node 4 NilT NilT) NilT)

3 + (sumTree (Node 4 NilT NilT)) + (sumTree NilT)
3 + (4 + (sumTree NilT) + (sumTree NilT)) + 0
3 + (4 + 0 + 0) + 0
7

depth (Node 3 (Node 4 NilT NilT) NilT)

1 + (depth (Node 4 NilT NilT)) + depth NilT
1 + (4 + depth NilT + depth NilT) + 0
1 + (1 + 0 + 0) + 0
2
-}

-- 14.20
-- Complete the redefinition of functions over Expr after it has been defined using the infix constructors

-- others defined above
sizeInfExpr :: InfixExpr -> Int
sizeInfExpr (InfixLit n) = 0
sizeInfExpr (e1 :+: e2) = 1 + sizeInfExpr e1 + sizeInfExpr e2
sizeInfExpr (e1 :-: e2) = 1 + sizeInfExpr e1 + sizeInfExpr e2

-- 14.21
-- Define functions to return the left- and right-hand sub-trees of an NTree.

data TreeSide = LeftSide | RightSide deriving Eq

returnTreeSide :: NTree -> TreeSide -> NTree
returnTreeSide NilT _ = NilT
returnTreeSide (Node n e1 e2) side
  | side == LeftSide = Node n (returnTreeSide e1 side) NilT
  | otherwise = Node n NilT (returnTreeSide e2 side)

-- I just write it, don't why, for future use probably
-- function to insert a leaf to the end of the tree
buildNewLeft :: NTree -> NTree -> NTree
buildNewLeft NilT leaf = leaf
buildNewLeft (Node n e1 e2) leaf
  | e1 == NilT = Node n leaf e2
  | otherwise = Node n (buildNewLeft e1 leaf) e2

-- 14.22
-- Define a function to decide whether a number is an element of an NTree
elemOfTree :: Int -> NTree -> Bool
elemOfTree _ NilT = False
elemOfTree num (Node n e1 e2)
  | num == n = True
  | otherwise = elemOfTree num e1 || elemOfTree num e2

-- 14.23
-- Define functions to find the maximum and minimmum values held in an NTree

-- helper function to get list of all elements of the list
getTreeElements :: NTree -> [Int]
getTreeElements NilT = []
getTreeElements (Node n t1 t2) = [n] ++ (getTreeElements t1) ++ (getTreeElements t2)

maxOfTree :: NTree -> Int
maxOfTree NilT = error "Empty Tree"
maxOfTree tree = foldr1 (\n acc -> if n > acc then n else acc) $ getTreeElements tree

minOfTree :: NTree -> Int
minOfTree NilT = error "Empty Tree"
minOfTree tree = foldr1 (\n acc -> if n < acc then n else acc) $ getTreeElements tree

-- 14.24
-- Define a function to reflect an NTree
-- What is the result of reflecting twice?
reflectTree :: NTree -> NTree
reflectTree NilT = NilT
reflectTree (Node n t1 t2) = Node n (reflectTree t2) (reflectTree t1)
-- Examplels of trees
tree1 = Node 0 (Node 1 (Node 3 NilT NilT) (Node 4 NilT NilT)) (Node 2 (Node 5 NilT NilT) (Node 6 NilT NilT))
tree2 = Node 0 (Node 2 (Node 6 NilT NilT) (Node 5 NilT NilT)) (Node 1 (Node 4 NilT NilT) (Node 3 NilT NilT))
-- Tests
prop1_reflectTree, prop2_reflectTree :: String
prop1_reflectTree
  | tree1 == (reflectTree tree2) = "First test passed."
  | otherwise = "First test not passed."
prop2_reflectTree
  | tree1 == (reflectTree $ reflectTree tree1) = "Second test passed."
  | otherwise = "Second test not passed."

-- 14.25
-- Define functions
collapseTree, sortTree :: NTree -> [Int]
-- TODO reforge this funciton to use a mutual recursive
collapseTree NilT = []
collapseTree (Node n t1 t2) = (collapseTree t1) ++ [n] ++ (collapseTree t2)

sortTree = sort . getTreeElements
-- I think I did it wrong, but who can check this?

-- 14.26
-- Complete the definitions of showPerson and showBiog
data PersonM = Adult Name Address Biog | Child Name
data Biog = Parent String [PersonM] | NonParent String

type Name = String
type Address = String

showPerson (Adult nm ad bio) = show nm ++ show ad ++ showBiog bio
showPerson (Child nm) = show nm

showBiog (Parent st perList) = st ++ concat (map showPerson perList)
showBiog (NonParent st) = st

-- 14.27
-- Extend the type Expr to that it contains conditional expressions

data Exp = Liter Int |
           Oper Ops Exp Exp |
           If BExp Exp Exp

data BExp = BoolLiter Bool |
            And BExp BExp |
            Not BExp |
            Equal Exp Exp |
            Greater Exp Exp

-- TODO Define functions by mutual recursion

-- eval :: Exp -> Int
-- evaluate Oper
-- other redirect if to bEval
-- If b e1 e2 *** If b has the value True then e1 otherwise e2

-- bEval :: BExp -> Bool
-- evaluate And Not
-- other redirect to eval
-- Greater e1 e2 *** is True when e1 is larger then e2

-- TODO Extend the function 'show' to show the redefined type of expressions

-- 14.28
-- Investigate which of the functions over trees discussed in the exercises of Section 14.2 can be made polymorphic.
{-
sumTree couldn't be polymorhic bc it sum integers in nods therefore we actually can't add other values but if we don't care about the length of the input so then we could append or zip values in a node of a tree

ofc functions depth could be polymorhic

and the same with the function occurs
-}

-- 14.29
-- Define a function twist which swaps the order of a union
twist :: Either a b -> Either b a
twist (Left a) = (Right a)
twist (Right a) = (Left a)

-- 14.30
-- How would you define applyLeft using the function either?
{-
applyLeft :: (a->c) -> Either a b -> c
applyLeft f (Left x) = f x
applyLeft f (Right _)  = error "applyLeft applied to Right"
-}
applyLeft :: (a -> c) -> Either a b -> c
applyLeft f x = either f (\n -> error "applyLeft aplied to Right") x

-- 14.31
-- Show that any funciton of type a -> b can be transformed into functions of type
{-
a -> Either b c
a -> Either c b

1stfunction a
  | a == n = Left b
  | otherwise = Right c
and vice versa
-}

-- 14.32
-- How could you generalize either to join so
joinEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
joinEither f g (Left a) = Left (f a)
joinEither f g (Right b) = Right (g b)

-- 14.33
-- Define functions
data GTree a = Leaf a | Gnode [GTree a] deriving (Show, Eq)

testtree = Gnode [Leaf 0,
                  Gnode [Leaf 1,
                         Gnode [Leaf 3, Leaf 4]],
                   Gnode [Leaf 2,
                          Gnode [Leaf 5, Leaf 6]]]

gCount, gDepth :: GTree a -> Int
-- count the number of leaves in a GTree
gCount (Leaf a) = 1
gCount (Gnode []) = 0
gCount (Gnode (x:xs)) = (gCount x) + (gCount (Gnode xs))

-- find the depth of a GTree
-- TODO I think this function contains a bug, so test it and don't forget about max function
gDepth (Leaf a) = 0
gDepth (Gnode []) = 0
gDepth (Gnode (x:xs)) = 1 + gDepth (Gnode xs)

-- sum numeric GTree Int
gSumTree :: GTree Int -> Int
gSumTree (Leaf a) = a
gSumTree (Gnode []) = 0
gSumTree (Gnode (x:xs)) = (gSumTree x) + (gSumTree (Gnode xs))

-- whether an element appers in a GTree
elemOfGTree :: Eq a => a -> GTree a -> Bool
elemOfGTree elm (Leaf a) = (a == elm)
elemOfGTree elm (Gnode []) = False
elemOfGTree elm (Gnode (x:xs)) = elemOfGTree elm x || elemOfGTree elm (Gnode xs)

-- map a function over the elements at the leaves of a GTree
mapGTree :: (a -> b) -> GTree a -> GTree b
mapGTree f (Leaf a) = Leaf (f a)
mapGTree f (Gnode xs) = mapHelper [] f (Gnode xs)
  where
    mapHelper new f (Gnode []) = Gnode new
    mapHelper new f (Gnode (x:xs)) = mapHelper (new ++ [mapGTree f x]) f (Gnode xs)
-- lol just made it on random. Is it a mutual recursion? xD

-- flatten a GTree to a list
flatGTree (Leaf a) = [a]
flatGTree (Gnode xs) = flatHelper [] (Gnode xs)
  where
    flatHelper list (Gnode []) = list
    flatHelper list (Gnode (x:xs)) = flatHelper (list ++ flatGTree x) (Gnode xs)
-- TODO think how would you reforge this function to increase perfomance

-- 14.34
-- How is the completely empty tree represented as a GTree
-- Gnode [] or Leaf []

-- *****
-- Error handling
-- *****
{-
1 Method just stop calculation and rise an error
   error :: String -> a
2 Method dummy values
   tail :: [a] -> [a]
   tail (_:xs) = xs
   tail [] = []
3 Method error values
   when condition holds, f gives an error
   fErr x
    | cond = Nothing
    | otherwise = just (f x)
-}

errDiv :: Int -> Int -> Maybe Int
errDiv n m
  | (m /= 0) = Just (n `div` m)
  | otherwise = Nothing

-- We can transmit an error value
-- this somehow dosen't work
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMyabe g Nothing = Nothing
mapMaybe g (Just x) = Just (g x)

-- this works as expected
data Perhabs a = Null | Something a
  deriving (Eq, Ord, Read, Show)

mapPerhabs :: (a -> b) -> Perhabs a -> Perhabs b
mapPerhabs g Null = Null
mapPerhabs g (Something a) = Something (g a)

-- or trap value
-- maybe :: b -> (a -> b) -> Maybe a -> b

-- 14.35
-- Define a function

-- takes nth and mth items of the list of numbers and returns their sum
-- return 0 if either of the numbers is not one of the indices from 0 to length-1
process :: [Int] -> Int -> Int -> Int
process list n m
  | nth == Nothing || mth == Nothing = 0
  | otherwise = nth `plus` mth
    where
      nth = takeIndex list n
      mth = takeIndex list m
      plus (Just x) (Just y) = x + y

takeIndex :: [a] -> Int -> Maybe a
takeIndex lon ind
  | (length lon)-1 < ind || (ind < 0) = Nothing
  | otherwise = findIndex lon ind 0
    where
      findIndex (x:xs) ind num
        | ind == num = Just x
        | otherwise = findIndex xs ind (num+1)

-- 14.36
-- Discuss the advantages and disadventages of the three approaches to error handling

-- 14.37
-- What are the values of type Maybe (Maybe a) - literally any values
-- Define a function
-- which squash Just (Just x) to Just x and all other values to Nothing
squashMaybe :: Maybe (Maybe a) -> Maybe a
squashMaybe (Nothing) = Nothing
squashMaybe (Just x) = x

-- 14.38
-- In a similar way to mapMaybe, define the function
composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe f g = squashMaybe . (mapMaybe g) . f

-- 14.39
data Err a = OK a | Error String
-- How do the definitions of mapMaybe, maybe and composeMaybe have to be modified to accommodate this new definititon?

mapErr :: (a -> b) -> Err a -> Err b
mapErr g (Error x) = Error x
mapErr g (OK a) = OK (g a)

err :: b -> (a -> b) -> Err a -> b
err n f (Error x) = n
err n f (OK x) = f x

squashErr :: Err (Err a) -> Err a
squashErr (Error x) = Error x
squashErr (OK a) = a

composeErr :: (a -> Err b) -> (b -> Err c) -> (a -> Err c)
composeErr f g = squashErr . (mapErr g) . f

-- TODO redo this exercise
