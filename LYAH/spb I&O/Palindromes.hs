respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "It is a palindrome!" else "HA! Not a palindrome.") . lines 
    where isPalindrome xs = xs == reverse xs

--main = interact respondPalindromes