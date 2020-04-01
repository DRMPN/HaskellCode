
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
