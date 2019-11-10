{-
Problem 39
(*) A list of prime numbers.
Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
Example in Haskell:
λ> primesR 10 20
[11,13,17,19]
-}

main = do print $ primesR 10 20

primesR :: Int -> Int -> [Int]
primesR s e = [x | x <- [s..e], isPrime x]

isPrime :: Int -> Bool
isPrime n
    | n <= 2 = True
    | otherwise = not $ any divisible [2 .. n `div `2]
        where divisible d = (n `mod` d) == 0
