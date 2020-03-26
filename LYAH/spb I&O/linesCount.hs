import System.Environment
import Control.Exception
import System.Directory
import System.IO.Error
import System.IO

oldmain = do 
    (fileName:_) <- getArgs
    fileExits <- doesFileExist fileName
    if fileExits
        then do contents <- readFile fileName
                putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines! "
        else do putStrLn "The file does not exist!"

-- with exceptions

main = toTry `catch` handler

toTry :: IO ()
toTry = do 
        (fileName:_) <- getArgs
        contents <- readFile fileName
        putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines! "

handler :: IOError -> IO ()
handler e 
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e

{-
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | isFullError e = freeSomeSpace  
    | isIllegalOperation e = notifyCops  
    | otherwise = ioError e  
-}
-- where notifyCops and freeSomeSpace are some I/O actions