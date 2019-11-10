{-
Problem 36
(**) Determine the prime factors of a given positive integer.
Construct a list containing the prime factors and their multiplicity.
Example:
* (prime-factors-mult 315)
((3 2) (5 1) (7 1))
Example in Haskell:
λ> prime_factors_mult 315
[(3,2),(5,1),(7,1)]
-}

import Data.List

main = do print $ prime_factors_mult 315

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
