{-
Problem 19
(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples:

* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)
Examples in Haskell:

λ> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"

λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}

main = do print $ rotate ['a','b','c','d','e','f','g','h'] 3
          print $ rotate ['a','b','c','d','e','f','g','h'] (-2)

rotate :: [a] -> Int -> [a]
rotate xs n = if n >= 0
    then drop n xs ++ take n xs
    else drop (length xs + n) xs ++ take (length xs + n) xs
