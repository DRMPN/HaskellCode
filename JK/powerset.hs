-- My interpretation for wiki's powerset
ps [] = [[]]
ps (x:xs) = ps xs ++ map(x:) (ps xs)

-- Completly translated powerset from wiki
psW [] = [[]]
psW (x:xs) = psW xs ++ [ t ++ [x] | t <- psW xs]

-- Sketch function for testing
sketch :: [a] -> [a]
sketch ls = [ l | l <- ls] 

-- Example function for sets
ex :: (b -> Bool) -> [a] -> [b] -> [(a,b)]
ex p ks xs = [ (k, x) | k <- ks, x <- xs, p x]