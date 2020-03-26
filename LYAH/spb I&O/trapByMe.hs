import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me your input! :"
    inp <- getLine
    putStrLn $ map toUpper inp