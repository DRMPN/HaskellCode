-- moved to another file cuz multiply definition error

--
--  Movable objects
--
data Vector = Vec Float Float

class Movable a where
  move :: Vector -> a -> a
  reflectX :: a -> a
  reflectY :: a -> a
  rotate180 :: a -> a
  rotate180 = reflectX . reflectY

data Point = Point Float Float deriving (Show)

-- to make point an instance of movable we have to give definitions
instance Movable Point where
  move (Vec v1 v2) (Point c1 c2) = Point (c1 + v1) (c2 + v2)
  reflectX (Point c1 c2) = Point c1 (-c2)
  reflectY (Point c1 c2) = Point (-c1) c2
  rotate180 (Point c1 c2) = Point (-c1) (-c2)

-- using the type of points we can build figures
data Figure = Line Point Point |
              Circle Point Float
              deriving (Show)

-- instance declaration of movable for figure
-- we use corresponding operations on Point
instance Movable Figure where
  move v (Line p1 p2) = Line (move v p1) (move v p2)
  move v (Circle p r) = Circle (move v p) r

  reflectX (Line p1 p2) = Line (reflectX p1) (reflectX p2)
  reflectX (Circle p r) = Circle (reflectX p) r

  reflectY (Line p1 p2) = Line (reflectY p1) (reflectY p2)
  reflectY (Circle p r) = Circle (reflectY p) r

-- for list of movable objects
instance Movable a => Movable [a] where
  move v = map (move v)
  reflectX = map reflectX
  reflectY = map reflectY

--
--  Named objects
--
class Named a where
  lookName :: a -> String
  giveName :: String -> a -> a

data Name a = Pair a String

instance Named (Name a) where
  lookName (Pair obj nm) = nm
  giveName nm (Pair obj _) = (Pair obj nm)

--
--  Putting together classes
--
mapName :: (a -> b) -> Name a -> Name b
mapName f (Pair obj nm) = Pair (f obj) nm

instance Movable a => Movable (Name a) where
  move v = mapName (move v)
  reflectX = mapName reflectX
  reflectY = mapName reflectY

class (Movable b, Named b) => NamedMovable b

instance Movable a => NamedMovable (Name a)
------------------------------------------------------------

-- Exercise 14.47
-- Complete the inctance declarations
instance (Movable b, Named c) => NamedMovable (b,c)

instance Movable b => Movable (b,c) where
  move v (b,c) = (move v b, c)
  reflectX (b,c) = (reflectX b, c)
  reflectY (b,c) = (reflectY b, c)

instance Named c => Named (b,c) where
  lookName (_, c) = lookName c
  giveName nm (obj, c) = (obj, giveName nm c)

-- Exercise 14.48
-- Show that the method of the previous question can be used to combine instances of any two classes
-- Ill use the induction and I would say that if it works for previous question then it works for any two classes.

-- Exercise 14.49
-- answer is the same

-- Exercise 14.50
-- Extend the collection of operations for moving objects to include scaling and rotation by an arbitary angle
-- This can be done by re-defining Movable or by defining a class MovablePlus over the class Movable.

class Movable a => MovablePlus a where
  scale :: Float -> a -> a
  rotate :: Float -> a -> a

instance MovablePlus Point where
  scale n (Point x y) = Point (n*x) y
  rotate n (Point x y) = Point x y

instance MovablePlus Figure where
  scale n (Line p1 p2) = Line p1 (scale n p2)
  scale n (Circle p r) = Circle p (n*r)
  -- we dont change x coordinate while rotating a line
  -- one way to solve - try to implement super formula
  -- another way - add an angle to line definition ez clap
  rotate n (Line p1 p2) = Line p1 p2
  rotate n (Circle p r) = Circle p r

-- Exercise 14.51
-- Design a collection of classes to model bank accounts.
-- These have different forms: current, deposit and so on, as well as different levels of functionality
-- Can you reuse the Named class here?

--------------------------------------------------------------
-- Information about person
--------------------------------------------------------------
data BankPerson = BankPerson Fname Lname
                  Sex
                  DateOfBirth
                  HomeAddress
                deriving (Show)

type Fname = String
type Lname = String
data Sex = M | F deriving (Show)
type DateOfBirth = String
type HomeAddress = String

--------------------------------------------------------------
-- remake to data and add some information about loan and etc
--------------------------------------------------------------
type Invoice = Int
type AccountNumber = Int

--------------------------------------------------------------
-- Bank account itself
--------------------------------------------------------------
data BankAccount = BankAccount BankAccountType
                   AccountNumber
                   BankPerson
                   Invoice

data BankAccountType = Individual | Business

--------------------------------------------------------------
-- Class definitions
--------------------------------------------------------------
