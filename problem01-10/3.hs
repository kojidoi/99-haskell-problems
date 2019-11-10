{-
Problem 3
(*) Find the K'th element of a list. The first element in the list is number 1.

Example:
* (element-at '(a b c d e) 3)
c

Example in Haskell:
λ> elementAt [1,2,3] 2
2
λ> elementAt "haskell" 5
'e'
-}

main = do print $ elementAt [1,2,3] 2
          print $ elementAt "haskell" 5

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
