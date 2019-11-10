{-
Problem 17
(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example:

* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
Example in Haskell:

λ> split "abcdefghik" 3
("abc", "defghik")
-}

main = do print $ split "abcdefghik" 3

_split :: ([a],[a]) -> Int -> ([a],[a])
_split (xs,[]) _ = (xs,[])
_split (xs,ys) 0 = (xs,ys)
_split (xs,y:ys) n = _split (xs ++ [y], ys) (n-1)

split :: [a] -> Int -> ([a],[a])
split xs n = _split ([],xs) n
