-- just to remember to myself :set +s

{-

         Understanding the problem
First we need to undersant the problemwe are trying to solve.
 - What are the inputs and outputs to the problem? Are there any special conditions on the inputs or outputs?
 - Looking at examples can help to clarify the problem.
 - Can the proble ve solved? Is the specification complete, or are there aspects which need clarification?
 - If there are fifferent possible ways of making sense of it, try to find out from the specifier what was inteded.
 - Does the problem itself have a structure? Is it made up of a nymber of parts which could be solved separately? Does a diagram help to describe the problem?

         Desinging a solution
Before writing a program we need to plan how we are going to do it.
 - Have you seen a similar problem before? If so, you might use its design as a guide.
 - Can you think of a simpler but related problem? If you can solve that, you might use or modify the solution.
 - Can you think of a generalization of the problem? This might be easier to solce that the original.
 - What is the architecture of the problem? Can you break it up into parts which may be solved (relatvely) independently? As well as the parts themselves you will need to think about how the parts fit together.
 - Think about how to go from the inputs to the output - a bottom-up approach; use the intermediate data as a guide. Also think about what resources you could be given which would let you solve the problem - this 'what if...?' approach is top-down.
 - Even at the planning stage it is important to know that your resources are. Make sure you check what is provided by your programming language and its libraries. Another inportant resource consists of the programs which you yourself have already written.
 - Design with change in mind. If your program is useful, then it will probably be modified a number of times over its lifetime.

          Writing a program
To write a program you need to be aware of the resources that your programming language  provides. You also need to follow the informal design or plan.
 - Haskell has a substantial number of library functions which support programming over lists. Some of these are general polymorphic higher-order functions which can be used in a large variety of situations. Try to use these if you can.
 - We shall see that over other data types we van define similar general functions. It is usually easier to use these functions than to write a solution from scratch.
 - You write your own general funcions by abstracting away from the particular. Specifically, the particular - like multiplying by two - can be turned into a function which becomes a parameter to the general functions (such as map).
 - Most languages allow you to make definitions with different cases; Haskell also provides pattern matching, which selects parts of an object as well as distinguishing cases.
 - Recursion is a general strategy for designing programs over data tyoes like lists and numbers. To define a recursive function f at argument x you need to ask 'what if I had the value of f at ...?'.
 - List comprehinsons provide an expressive notation for lists.
 - You may need to introduce other functions as you begin to write your definitions. These might appear in where clauses or at the top level of the program.
 - If you cannot define the function you need to, try a simpler one. A solution to this might be a model for what you want, or could be used in the definition of the final funcion.

         Reflection
Looking back on what you have done might affect your program, its design or indeed the specification of the problem itself.
 - Can you test your solution> You need to think of the testing groups over which the program should show similar behavior, as well as looking hard at any special cases.
 - If your testing reveals errors or 'bugs', try to find their source. Are errors due to accidental mistakes? problems in understanding how the Haskell language works? misunderstanding how to solve the problem? misunderstanding the problem itself? or some other reason?
 - You can learn from the errors you have made; try keeping a log of all the errors that you make, and the reason for the,. This should help you not to repeat them.
 - Can you prove that your program does what it should? If not, you can ask why this is, and whether it pointsto errors in the program or the design.
 - Suppose you were asked to write the same program again. How would you do it differently?
 - Suppose you were asked to modify or extend the program. How easy would that be? If it is difficult, can you think how you might have designed or written the solution differently to accommodate changes more readily?
 - Does the program run in reasinable time? If not, can you see where the bottlenecks are? Can you see how to modify the program to improve its performane?

-}


-- 11.1
-- Give a recursive definition of the range
-- [m,n .. p]
{-
1 case
m>n ->
  m > p = []
  m : [(n, n+(m-n) .. p]
2 case
m<n ->
  m < p = []
  m : [n, n-(m-n) .. p]
-}

-- 11.2
-- Think of two more ways of implementing the function
{-
simplePalCheck :: String -> Bool
simplePalCheck st = ( reverse st ) == st

1) Modify it to sove
2) Use it to a simplified problem
3) Rewrite it using different style e.g list comprehinson, function composition, higher-order functions
4) Find and use a function/functions in libraries i.e you won't write it from scratch, just net surfing in hoogle and composing in one functions ...
-}

-- 11.3
-- Define a function
-- Option 1: If we are sure, that the input will be a sentense without punctuation marks
subst :: String -> String -> String -> String
subst text old new
  | null old || null new = text
  | otherwise = unwords changeWord
  where changeWord = start ++ [new] ++ finish
        start = takeWhile (/=old) toWords
        finish = tail $ dropWhile (/=old) toWords
        toWords = words text

-- bug or feature? - |#|  subst "some words to me written" "words" ""   |#|
-- ** fixed :^> **

-- TODO rewrite that function will change old word to new word when its checking that the old word is subSequence of text
-- Option 2: If we want to change a word in sentence, which contains punctuatin marks
-- modify the function from exercise 7 or write new one. Main problem : need to save too many variables
