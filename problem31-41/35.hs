{-
Problem 35
(**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
Example:
* (prime-factors 315)
(3 3 5 7)
Example in Haskell:
λ> primeFactors 315
[3, 3, 5, 7]
-}

main = do print $ primeFactors 315

primeFactors :: Int -> [Int]
primeFactors n = _primeFactors n []

_primeFactors :: Int -> [Int] -> [Int]
_primeFactors 1 xs = xs
_primeFactors n xs = if factors == []
                     then xs
                     else let factor = head factors
                          in _primeFactors (n `div` factor) (xs ++ [factor])
    where factors = [x | x <- [2 .. n], n `mod` x == 0]
