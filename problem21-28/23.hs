{-
Problem 23
Extract a given number of randomly selected elements from a list.

Example:

* (rnd-select '(a b c d e f g h) 3)
(E D A)
Example in Haskell:

λ> rnd_select "abcdefgh" 3 >>= putStrLn
eda
-}

import System.Random

 {-
 main :: IO ()
main = do
    x <- randomRIO (0, 1) :: IO Double
    putStrLn (show x)
-}

main = do rnd_select "abcdefgh" 3 >>= putStrLn

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs 1 = do
    i <- randomRIO (0, (length xs)-1)
    return [xs !! i]
rnd_select xs n = do
    ys <- rnd_select xs (n-1)
    i <- randomRIO (0, (length xs)-1)
    return (ys ++ [xs !! i])
