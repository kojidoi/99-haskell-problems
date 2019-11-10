{-
Problem 14
(*) Duplicate the elements of a list.

Example:

* (dupli '(a b c c d))
(A A B B C C C C D D)
Example in Haskell:

λ> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}

main = do print $ dupli [1, 2, 3]

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ dupli xs
