{-
Problem 5
(*) Reverse a list.

Example in Haskell:
λ> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
λ> myReverse [1,2,3,4]
[4,3,2,1]
-}

main = do print $ myReverse "A man, a plan, a canal, panama!"
          print $ myReverse [1,2,3,4]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [a] = [a]
myReverse (x:xs) = myReverse xs ++ [x]
