{-
Problem 12
(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:

λ> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}

main = do let xs = [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
          print $ decodeModified xs

data Elem a = Single a | Multiple Int a

decodeModified :: [Elem a] -> [a]
decodeModified [Single x] = [x]
decodeModified [Multiple n x] = replicate n x
decodeModified (x:xs) = decodeModified [x] ++ decodeModified xs
