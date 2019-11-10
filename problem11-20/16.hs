{-
Problem 16
(**) Drop every N'th element from a list.

Example:

* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)
Example in Haskell:

λ> dropEvery "abcdefghik" 3
"abdeghk"
-}

main = do print $ dropEvery "abcdefghik" 3

_dropEvery :: [a] -> Int -> Int -> [a]
_dropEvery [] _ _ = []
_dropEvery (x:xs) n i = if n == i
    then _dropEvery xs n 1
    else x : _dropEvery xs n (i+1)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = _dropEvery xs n 1
