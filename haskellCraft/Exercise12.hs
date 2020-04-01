
{-
** NOT OVERLOADED **
elemInt :: Int -> [Int] -> Bool
Problem: Each time we want to check membership of a list of a different type we will have to define yet another - very similar - function.
elemBool :: Bool -> [Bool] -> Bool

Way out of this problem:
** OVERLOADED **
elem :: Eq a => a -> [a] -> Bool
-}
