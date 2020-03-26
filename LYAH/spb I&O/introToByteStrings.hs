import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import System.Environment

-- B.unpack $ B.pack [98,97,110]
-- B.toChunks  $ B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]

-- cons is lazy so it will make a new chunk even if the first chunk isn't full
-- better to use strict version - cons'
-- foldr B.cons B.empty [50..60]
-- foldr B.cons' B.empty [50..60]

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents

-- if program reads a lot of data into strings, give bytestrings a try