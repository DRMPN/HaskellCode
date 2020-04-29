-- 15.1 skipped

-- 15.2
-- Explain why you think is is the default that imported definitions are not themselves exported.
-- I think it's because of code's safety.

-- 15.3 skipped

-- Exercise 15.4
-- What is the coding of the message battat using the following tree?
{-
 /\
b /\
 a  t
-}

-- L RL RR RR RL RR
-- lenght is 11 while the others is 10 and 9

-- Exercise 15.5
-- Using the first coding tree decode the coded message
-- RL L RL RL L RR
{-
 /\
a /\
 b  t
-}

-- babbat

-- Which tree would you expect to give the best coding of the message?
-- Tree from exercise 15.3 because in the message b is domiating and the tree has one length "bit" for b
