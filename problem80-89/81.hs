{-
Problem 81
(**) Path from one node to another one

Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.

Example in Haskell:

λ> paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[1,2,3,4],[1,3,4]]
λ> paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
-}

main = do
    print $ paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print $ paths 2 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

paths :: Int -> Int -> [(Int,Int)] -> [[Int]]
paths a b graph = filter (/=[]) $ paths' a b graph []

paths' :: Int -> Int -> [(Int,Int)] -> [Int] -> [[Int]]
paths' a b graph history
    | a == b           = [history ++ [b]]
    | a `elem` history = []
    | otherwise        = concat $ [paths' adj b graph (history++[a]) | adj <- dadjs a graph]

dadjs :: Int -> [(Int,Int)] -> [Int]
dadjs _ [] = []
dadjs node (x:xs)
    | a == node = b:(dadjs node xs)
    | otherwise = dadjs node xs
    where (a,b) = x
