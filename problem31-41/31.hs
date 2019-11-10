{-
Problem 31
(**) Determine whether a given integer number is prime.
Example:
* (is-prime 7)
T
Example in Haskell:
λ> isPrime 7
True
-}

main = do
    print $ isPrime 7

isPrime :: Int -> Bool
isPrime n
    | n <= 2 = True
    | otherwise = not $ any divisible [2 .. n `div `2]
        where divisible d = (n `mod` d) == 0
