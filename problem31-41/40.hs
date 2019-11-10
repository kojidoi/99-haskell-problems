{-
Problem 40
(**) Goldbach's conjecture.
Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
Example:
* (goldbach 28)
(5 23)
Example in Haskell:
λ> goldbach 28
(5, 23)
-}

-- https://ja.wikipedia.org/wiki/ゴールドバッハの予想

main = do print $ goldbach 28

goldbach :: Int -> (Int,Int)
goldbach n = head [(p,q) | p<-primes, q<-primes, p+q==n] where primes = primesR 2 n


primesR :: Int -> Int -> [Int]
primesR s e = [x | x <- [s..e], isPrime x]

isPrime :: Int -> Bool
isPrime n
    | n <= 2 = True
    | otherwise = not $ any divisible [2 .. n `div `2]
        where divisible d = (n `mod` d) == 0
