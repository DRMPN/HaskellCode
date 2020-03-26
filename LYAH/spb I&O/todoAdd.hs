import System.IO
import System.Directory
import Data.List

-- to rework this with apppend
-- think about that appendFile fileName (todoItem ++ "/n")

main = do
    -- open todo list
    handle <- openFile "todo1.txt" ReadMode
    -- create temp file list
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    --something
    putStrLn " "
    putStrLn "Welcome to the add to-do item program!"
    putStr "Please, enter a desirable unit: "
    newItem <- getLine
    -- append todo item to a list
    let todoTasks = lines contents
    let newContents = newItem : todoTasks
    -- change files
    hPutStr tempHandle $ unlines newContents
    hClose handle 
    hClose tempHandle
    removeFile "todo1.txt"
    renameFile tempName "todo1.txt"
    putStrLn "Done"