{-
Problem 24
Lotto: Draw N different random numbers from the set 1..M.

Example:

* (rnd-select 6 49)
(23 1 17 33 21 37)
Example in Haskell:

λ> diff_select 6 49
[23,1,17,33,21,37]
-}

-- Make them different!!!

import System.Random

main = do diff_select 6 49 >>= print

diff_select :: Int -> Int -> IO [Int]
diff_select 1 m = do
    x <- randomRIO (1, m)
    return [x]
diff_select n m = do
    xs <- diff_select (n-1) m
    x <- randomRIO (1, m)
    return (x:xs)

