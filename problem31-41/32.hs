{-
Problem 32
(**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
Example:
* (gcd 36 63)
9
Example in Haskell:
λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
-}

main = do print $ [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]

myGCD :: Int -> Int -> Int
myGCD a b = if r == 0
            then b
            else myGCD b r
    where r = a `mod` b

-- 例題2が合わない：-3
