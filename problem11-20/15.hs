{-
Problem 15
(**) Replicate the elements of a list a given number of times.

Example:

* (repli '(a b c) 3)
(A A A B B B C C C)
Example in Haskell:

λ> repli "abc" 3
"aaabbbccc"
-}

main = do print $ repli "abc" 3

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ repli xs n
