import Control.Monad(when)
import System.Random

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number is the range from 1 to 10 am I thinking of? Tell me: "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "YES! You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        askForNumber newGen