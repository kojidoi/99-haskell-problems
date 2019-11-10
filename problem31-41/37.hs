{-
Problem 37
(**) Calculate Euler's totient function phi(m) (improved).
See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...
Note that a ** b stands for the b'th power of a.
-}

import Data.List

main = do print $ totient 10

totient :: Int -> Int
totient 1 = 1
totient m' = product [(p-1)*p^(m-1) | (p,m) <- prime_factors_mult(m')]


prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult n = map (\xs->(head xs,length xs)) (group $ primeFactors n)

primeFactors :: Int -> [Int]
primeFactors n = _primeFactors n []

_primeFactors :: Int -> [Int] -> [Int]
_primeFactors 1 xs = xs
_primeFactors n xs = if factors == []
                     then xs
                     else let factor = head factors
                          in _primeFactors (n `div` factor) (xs ++ [factor])
    where factors = [x | x <- [2 .. n], n `mod` x == 0]
