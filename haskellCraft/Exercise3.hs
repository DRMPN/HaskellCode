
-- not equal
mystery :: Int -> Int -> Int -> Bool
mystery m n p = not $ m==n && n==p

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = m/=n && m/=p && n/=p

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p b = m==n && m==p && m==b && n==p && n==b && p==b

min' :: Int -> Int -> Int
min' x y
  | x > y = y
  | otherwise = x

minThree :: Int -> Int -> Int -> Int
minThree x y z
  | x<y && x<z = x
  | y<z = y
  | otherwise = z

