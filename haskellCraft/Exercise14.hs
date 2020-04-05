-- :set +s

import Test.QuickCheck (quickCheck)

-------------------------------------------------------------

-- nullary as they take no arguments
data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter deriving (Eq, Ord, Enum, Show, Read) -- added later

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

data Shape = Circle Float | Rectangle Float Float deriving (Eq, Ord, Show, Read) -- added later

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle h w) = h * w

{-
Circle :: Float -> Shape
General definition of that is :
Con1 :: ti1 -> Typename

  The types can be recursive; we can use the type we are defining, Typename, as (part of) any of the types tij. This gives us lists, trees and many other data structures.
The Typename can be followed by one or more type variables which may be used on the right-hand side, making the definition polymorphic.

Contrasting type and data definitions:
  A synonym given by type is simply a shorthand, and so synonym type can always be expanded out, and thefore removed from the program. On the other hand, a data definition creates a new type. Because sumonyms are simply shorthand, a synonym definition cannot be recursive.
-}
