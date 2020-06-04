module Ex5_10 where
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
