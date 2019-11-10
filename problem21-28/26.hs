{-}
Problem 26
(**) Generate the combinations of K distinct objects chosen from the N elements of a list

In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:

* (combinations 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )
Example in Haskell:

λ> combinations 3 "abcdef"
["abc","abd","abe",...]
-}

main = do print $ combinations 3 "abcdef"

combinations :: Int -> [a] -> [[a]]
combinations 0 xs = [[]]
combinations 1 xs = [[x] | x <- xs]
-- combinations 2 (x:xs) = [x:y | y <- (combinations 1 xs)]
combinations k (x:xs) = [x:y | y <- (combinations (k-1) xs)]
