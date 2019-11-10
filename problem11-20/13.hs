{-
Problem 13
(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:

* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:

λ> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

main = do print $ decodeModified $ encodeDirect "aaaabccaadeeee"

data Elem a = Single a | Multiple Int a

_enc1 :: Eq a => Elem a -> a -> [Elem a]
_enc1 (Single x) y = if x == y
    then [Multiple 2 x]
    else [Single x, Single y]
_enc1 (Multiple n x) y = if x == y
    then [Multiple (n+1) x]
    else [Multiple n x, Single y]

_encode :: Eq a => [Elem a] -> [a] -> [Elem a]
_encode xs [] = xs
_encode xs (y:ys) = _encode (init xs ++ _enc1 (last xs) y) ys

encodeDirect :: Eq a => [a] -> [Elem a]
encodeDirect [] = []
encodeDirect (x:xs) = _encode [Single x] xs

-- from problem 12
decodeModified :: [Elem a] -> [a]
decodeModified [Single x] = [x]
decodeModified [Multiple n x] = replicate n x
decodeModified (x:xs) = decodeModified [x] ++ decodeModified xs
