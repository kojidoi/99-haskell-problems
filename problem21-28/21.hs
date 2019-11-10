{-
Problem 21
Insert an element at a given position into a list.

Example:

* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
Example in Haskell:

λ> insertAt 'X' "abcd" 2
"aXbcd"
-}

main = do print $ insertAt 'X' "abcd" 2

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n = take (n-1) ys ++ [x] ++ drop (n-1) ys
