import System.IO

main = do
    handle <- openFile "badSong.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

main' = do
    withFile "badSong.txt" ReadMode (\handle -> do 
        contents <- hGetContents handle 
        putStr contents
        )