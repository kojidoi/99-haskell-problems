{-
Problem 25
Generate a random permutation of the elements of a list.

Example:

* (rnd-permu '(a b c d e f))
(B A D C E F)
Example in Haskell:

λ> rnd_permu "abcdef"
"badcef"
-}

import System.Random

main = do rnd_permu "abcdef" >>= print

rnd_permu :: [a] -> IO [a]
rnd_permu [] = do return []
rnd_permu [x] = do return [x]
rnd_permu xs = do
    i <- randomRIO (0, (length xs)-1)
    let x = xs !! i
    let ys = (take (i-1) xs) ++ (drop i xs)
    zs <- rnd_permu ys
    return (x:zs)
