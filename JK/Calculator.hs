--------------------------------------------------
-- Variables
--------------------------------------------------
eq = [   (1,  0.4), (2,  0.24), (3,  0.144), (4,  0.216)    ]

main = calc eq

--------------------------------------------------
-- Calculator
--------------------------------------------------

calc a = printResult . fromMaybe . helper a [] [] $ 0
 where
  fromMaybe (Just x) = x
  fromMaybe (Nothing) = (0,0)


helper [] mx mx2 check 
 | check /= 1 = Nothing
 | otherwise = Just (sum mx, sum mx2)
helper ((x,p):xs) mx mx2 ch = helper xs (mx++[x*p]) (mx2 ++ [x^2*p]) (ch + p)


printResult (mx, mx2) = 
 do
  putStrLn " "
  putStrLn $ "Expected x - " ++ show (mx::Float)
  putStrLn $ "Exp in x^2 - " ++ show (mx2::Float)
  putStrLn " "
  putStrLn $ "Dispersion - " ++ show (disp::Float)
  putStrLn $ "Average x  - " ++ show (sqrt disp)
  putStrLn " "
 where 
  disp = (mx2 - mx^2)

--------------------------------------------------
-- Graph
--------------------------------------------------

-- use easyploat
