
-- :set +s

-- 13.1
-- Predict the type errors you would obrain by defining the following functions
{-
Different input type, n must be in Num n
f n = 37 + n
f True = 34

Different output type, funciton must produce eiter Num or Bool
g 0 = 37
g n = True

Same thing
h x
  | x>0 = True
  | otherwise = 37

x must be a consistent with Num, so you can't do k True
k x = 34
k 0 = 35
-}

-- Crucial point is that the definition of a funciton in not permitted to force any of its arguments to be polymorphic.

-- 13.2
-- Do the following paors of types - listed vertically - unify? If so, give a most general unifier for the,; if not, explain why they fail to unify.
{-

Can be unified
(Int -> b)
(a -> Bool)
- Most general unifier (monomorphic) (Int -> Bool)

Can be unified too
(Int, a, a)
(a, a, [Bool])
- Most general unifier (Int, a, [Bool])
-}

-- 13.3
-- Show the we can unify (a, [a]) with (b, c) to give (Bool, [Bool])
{-
  1. Second argument of the tuple must contain the same type as first argument and be a list of that type.
  2. First and second argument must contain different types
 With that information we can unify it to get general type - (t,[t]), therefore we can get type (Bool, [Bool])
-}

-- 13.4
{-
Can the function
f :: (a, [a]) -> b
be applied to the arguments 1. (2,[3]), 2. (2,[]), and 3. (2,[True]); if so, what the types of the results? Explain your answers.

1. f :: Num a => (a, [a]) -> b
2. the same thing
3. Error can't constraint a function with that definition bc <a> must have the same type
-}

-- 13.5
-- Repeat the previous question fo the function
-- ff :: (a, [a]) -> a
{-
There will be no difference between my answers bc function's input definition stays unchanged, only one thing has changed and it is output, it means that function will give to us output with the same type as input
-}

-- 13.6
-- Give the type of f [] [] if f has type
-- f :: [a] -> [b] -> a -> b
{-
type of the function (at my point of view) stays unchanges bc [] is super polymorphic input
-}

-- What is the type of the function h given by the definition
-- h x = f x x
{-
Type is h :: a -> b (most general as I think)
        that's bc we have another function f which might or might not change our input to another type
-}

-- 13.7
-- How can you use the Haskell system to check whether two type expressions are unifiable, and if so what is their unification?

-- we can use :t to check that, if type expression has type t then it's monomorphic and we (probably, I'm not sure) cannot unification with it.
zircon = zircon

-- 13.8
-- bla bla bla... variable function in that examples changes type of the main function and sometimes varFunc can't do that due to mismatching of types

-- 13.9
-- Give an algorithm which decides whether two type expressions are unifiable.
{-
It's hard for me to understand what is a unifiable (I'm lazy) but my general algorithm is :
 1. Both type expressions must not be a monomorphic
 2. Either of them must have the same amount of arguments
 3. something else :^)
-}

-- 13.10
-- Give the type of each of the individual conditional equations which follow, and discuss the type of the function which together they define.
{-
merge (x:xs) (y:ys)
Eq a => [a] -> [a] -> [a]

merge (x:xs) [] = (x:xs)
[a] -> [] -> [a]

merge [] (y:ys) = (y:ys)
[] -> [a] -> [a]

merge [] [] = []
[] -> [] -> ->[]
-}

-- 13.11
-- Define a polymorphic sorting function, and show how its type is derived from the type of the ordering relation
polySort :: Ord a => [a] -> [a]
polySort [] = []
polySort (x:xs) = smaller ++ [x] ++ bigger
  where
    smaller = polySort . filter (<=x) $ xs
    bigger = polySort . filter (>x) $ xs
-- In two words: functions < and <= are both in Ord class, it means that a variable, passed to this functions, must be in class Ord to proceed evaluation

-- 13.12
-- Investigate the types of the following numerical funcitons; you will find that the types refer to some of the built-in numeric classes.
{-
mult x y = x*y
:t (*) :: Num a => a -> a ->a
mult :: Num a => a -> a -> a

divide x = x `div` 2
:t (div) :: (Num a, Eq a, Enum a) => (Num a, Ord a, Enum a) => (Real a, Enum a) => Integral a => a -> a -> a

share x = x / 2.0
:t (/) :: Num a => Fractional a => a -> a -> a
-}
