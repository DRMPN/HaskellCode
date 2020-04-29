module Coding where

import Types

-- look up the value corresponding to a 'key' in a table.
lookupTable :: Table -> Char -> HCode
lookupTable [] c = error "lookupTable"
lookupTable ( (ch, n) : tb ) c
  | ch == c = n
  | otherwise = lookupTable tb c

-- look up each character in the table,
-- and concatenate the results.
codeMessage :: Table -> [Char] -> HCode
codeMessage tbl = concat . map (lookupTable tbl)

-- Exercise 15.7
-- With the table
table1 = [ ('a', [L]), ('b', [R,L]), ('t', [R,R]) ]
-- give a calculation of
-- codeMessage table1 "battab"
{-
codeMessage table1 "battab"
concat . map (lookupTable table1) "battab"
  lookupTable ('a', [L]) : ... 'b'
    'a' == 'b'
  lookupTable ('b' [R,l]) : ... 'b'
    'b' == 'b'
concat . [R,L] : map (lookupTable table1) "attab"
  lookupTable ('a', [L]) : ... 'a'
    'a' == 'a'
concat . [R,L] : [L] : map (lookupTable table1) "ttab"
  lookupTable ('a', [L]) : ... 't'
    'a' == 't'
  lookupTable ('a', [R,L]) : ... 't'
    'b' == 't'
  lookupTablle ('t', [R,R]) : [] 't'
    't' == 't'
concat . [R,L] : [L] : [R,R] : map (lookupTable table1) "tab"
concat . [R,L] : [L] : [R,R] : [R,R] : map (lookupTable table1) "ab"
concat . [R,L] : [L] : [R,R] : [R,R] : [L] : map (lookupTable table1) "b"
concat . [R,L] : [L] : [R,R] : [R,R] : [L] : [R,L] : map (lookupTable table1) []
concat . [ [R,L], [L], [R,R], [R,R], [L], [R,L] ]
[R,L,L,R,R,R,R,L,R,L]
-}
