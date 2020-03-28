

-- 8.1
{-

1) (4>2) || fac(-2) == 17
2) (4>2) && fac(-2) == 17

so 1) exp will give us True, but 2) will give us infinite loop or undefined value. You probably want to ask : What's wrong with them? The problem is quite easy to understand if you keep in mind laziness of language. You need to think about possible cases of || function and what it should produce i.e first try False || False || True then something like True || undefined . That's the answer! ( It will always stop when it reaches True)

-}

-- 8.2
{-
This function won't break or stack if argument would be like:
0 fac(n-2)
that's because we filter unwanted results BEFORE main action
fac(-2) 0
that will break because we firstly check first argument and then second one
-}
mult :: Int -> Int -> Int
mult a b
  | a == 0 || b == 0 = 0
  | otherwise = a * b

-- 8.3
-- prove for all finite xs and ys

{-
dunno how to do it, so it's just my improvisation

sum (xs ++ ys) = sum xs + sum ys
sum ([] ++ xs) = sum [] + sum xs = 0 + sum xs = sum xs
sum ((x:xs) ++ ys) = sum (x:xs) + sum ys = sum x + sum xs + sum ys = 1 + sum (xs ++ ys) = 1 + sum xs + sum ys
something like this probably

-}

-- 8.4
-- skipped. i think its not healty for me right now

-- 8.5 - 8.10 (!!! omitted !!! )
-- see 8.4

