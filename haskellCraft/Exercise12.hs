
{-
** NOT OVERLOADED **
elemInt :: Int -> [Int] -> Bool
Problem: Each time we want to check membership of a list of a different type we will have to define yet another - very similar - function.
elemBool :: Bool -> [Bool] -> Bool

Way out of this problem:
** OVERLOADED **
elem :: Eq a => a -> [a] -> Bool
-}

-- 12.1
-- How would you define /= from equality? What is the type of /=?
{-
(/=) :: Eq a => a -> a -> Bool
(/=) a b = not $ (==) a b
-}

-- 12.2
-- Define function numEqual which takes a list of items and an item, and returns the number of times x occurs in xs
-- I think it will be - numEqual :: Eq a => [a] -> a -> Int
-- actual type
numEqual :: (Num n, Eq a) => [a] -> a -> n
numEqual [] _ = 0
numEqual (x:xs) a
  | x == a = 1 + numEqual xs a
  | otherwise = 0 + numEqual xs a

-- probably I missunderstanding something about Num type

-- 12.3
-- define functions
-- If a list doesn't contain a pair that we are looking for, then we don't know which element of a list function should produce, therefore with our type declaration we have to produce element b, and the only way to do this is to make an exception. (As I think so)
oneLookupFirst :: Eq a => [(a,b)] -> a -> b
oneLookupFirst [] a = error "No such element in the list."
oneLookupFirst (x:xs) a
  | a == fst x = snd x
  | otherwise = oneLookupFirst xs a

oneLookupSecond :: Eq b => [(a,b)] -> b -> a
oneLookupSecond [] a = error "No such element in the list."
oneLookupSecond (x:xs) b
  | b == snd x = fst x
  | otherwise = oneLookupSecond xs b

-- another way to define those functions
oneLookUpFirstFoldr list a = foldr (\(x,y) acc -> if x == a then y else acc) (error "No such element in the list") list

---------------------------------------------------------------------------------------

class Visible a where
  toString :: a -> String
  size :: a -> Int

instance Visible Char where
  toString ch = [ch]
  size _ = 1

instance Visible Bool where
  toString True = "True"
  toString False = "False"
  size _ = 1

instance Visible a => Visible [a] where
  toString = concat . map toString
  size = foldr (+) 1 . map size

-- 12.4
-- How would you make Bool, pair types (a, b), and triple types, (a, b, c), into Visible types?

instance (Visible a, Visible b) => Visible (a, b) where
  toString (a, b) = (++) (toString a) (toString b)
  size _ = 1

instance (Visible a, Visible b, Visible c) => Visible (a, b, c) where
  toString (a, b, c) = (toString a) ++ (toString b) ++ (toString c)
  size _ = 1

-- 12.5
-- Write a function to convert an integer into a String, and hence show how Int can be an instance of Visible

instance Visible Int where
  toString = show
  size _ = 1

-- 12.6
-- What is the type of the function
-- compare x y = size x <= size y
-- compare :: (Visible a, Visible b) => a -> b -> Bool

-- 12.7
-- Complete the default definitions for the class Ord
{-
x >= y = x > y || x == y
x < y = y > x
min x y = \x y -> if x > y && y < x then x else y
max x y = \x y -> if x < y && y > x then x else y
compare x y = \x y -> | x > y = GT | x < y = LT | otherwise = EQ
-}

-- 12.8
-- complete the following instance declarations
{-
instance (Ord a, Ord b) => Ord (a,b) where

instance Ord b => Ord [b] where
-}

-- 12.9
-- Investigate the Haskell definition of '<' on the type Bool and (t1,t2,...,tk)

-- 12.10
-- Define a function TODO investigate what the hell is this
--showBoolFun :: (Bool -> Bool) -> String
-- once again shitty exercises

-- Generalize this to
-- showBoolFunGen :: (a -> String) -> (Bool -> a) -> String

{-
                            ** Short summary **

Overloading - functions and operators can have different definitions as different types
   -- mechanism which enables this is the system of classes

A class definition contains a signature which contains the names and types of operations which must be supplied if a type is to be a member of the class. For a particular type. the function definitions are contained in an instance declaration.

-}
