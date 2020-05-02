module MakeCode where

import Types
import Frequency (frequency)
import MakeTree (makeTree)
import CodeTable (codeTable)

codes :: [Char] -> Tree
codes = makeTree . frequency
