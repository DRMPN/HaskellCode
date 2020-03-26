import Control.Monad
import Data.Char

main = do
  putStr "Type something: "
  line <- getLine
  if null line
    then do return ()
    else do
    putStrLn $ reverseWords line
    main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
-- ===========================================

char' = do
  c <- getChar
  if c /= ' '
    then
    do
      putChar c
      char'
    else
    do
      return ()

char = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    char
-- ===========================================

seq' = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

seq = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
-- ===========================================

frvr = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

f0rM = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
