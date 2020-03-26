import System.Random

-- random (mkStdGen 100) :: (Int, StdGen)  

treeCoins :: StdGen -> (Bool, Bool, Bool)
treeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)