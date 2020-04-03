
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
