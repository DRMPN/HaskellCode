middleNumber :: Int -> Int -> Int -> Int
middleNumber x y z
  | y <= x && x <= z = x
  | x <= y && y <= z = y
  | otherwise = z

maxFour1 :: Int -> Int -> Int -> Int -> Int
maxFour1 m n p b
  | m > n && m > p && m > p && m > b = m
  | n > p && n > b = n
  | p > b = p
  | otherwise = b

maxFour2 :: Int -> Int -> Int -> Int -> Int
maxFour2 m n p b =
  max (max m n) (max p b)

maxFour3 :: Int -> Int -> Int -> Int -> Int
maxFour3 m n p b =
  max (maxThree m n p) b

maxThree :: Int -> Int -> Int -> Int
maxThree x y z
  | x >= y && x >= z    = x
  | y >= z              = y
  | otherwise = z

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual m n p
  |(m==n) && (n==p) = 3
  | m == n || m == p || n == p = 2
  | otherwise = 0

howManyEqual' :: Int -> Int -> Int -> Int
howManyEqual' a b c
  | threeEqual a b c = 3
  | threeDiff a b c = 0
  | otherwise = 2

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c = a==b && b==c

threeDiff :: Int -> Int -> Int -> Bool
threeDiff a b c = a/=b && b/=c

howManyOfFourEqual :: Int -> Int -> Int -> Int -> Int
howManyOfFourEqual a b c d
  | fourEqual a b c d = 4
  | fourDiff a b c d = 0
  | threeOfFourEqual a b c d = 3
  | otherwise = 2

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = a==b && threeEqual b c d

fourDiff :: Int -> Int -> Int -> Int -> Bool
fourDiff a b c d = a/=b && threeDiff b c d

threeOfFourEqual :: Int -> Int -> Int -> Int -> Bool
threeOfFourEqual a b c d = (threeEqual a b c) || (threeEqual a b d) || (threeEqual a c d) || (threeEqual b c d)



rangeProduct :: Int -> Int -> Int
rangeProduct m n
  | m == n = 1
  | m < n = m * (rangeProduct (m+1) n)
  | otherwise = 0

fac :: Int -> Int
fac a = rangeProduct 1 a



mult :: Int -> Int -> Int
mult a b
  | b == 0 = 0
  | b > 0 = mult a (b-1) + a
  | otherwise = 0

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage m n p =
  let average = (m + n + p) `div` 3
  in length . filter (\x -> x > average) $ [m,n,p]

-- 5.10

{-

divisors 12
[1,2,3,4,6,12]

divisors 7
[1,7]

foldr (\x y -> (x+1):y ) [7] [1,2,3,4,5]
foldr (\x acc -> if (isZero . mod 12 $ x) then x:acc else acc) [12] [1..9]

isZero :: Integral a => a -> Bool
isZero = (==0)

-}


--divisors :: Integral a => a -> [a]
divisors :: Int -> [Int]
divisors n
  | n > 0 && n <= 9 = foldr (\x acc -> if ((==0) . mod n $ x) then x:acc else acc) [] [1..9]
  | n > 9 = foldr (\x acc -> if ((==0) . mod n $ x) then x:acc else acc) [n] [1..9]
  | otherwise = []

isPrime :: Int -> Bool
isPrime = (==2) . length . divisors
