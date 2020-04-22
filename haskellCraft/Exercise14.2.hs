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
