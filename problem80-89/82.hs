{-
Problem 82
(*) Cycle from a given node

Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The predicate should return all cycles via backtracking.

Example in Haskell:

λ> cycle 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[[2,3,4,2]]
λ> cycle 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
[]
-}

main = do
    print $ cycle' 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print $ cycle' 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]

cycle' :: Int -> [(Int,Int)] -> [[Int]]
cycle' a graph = paths a a graph

--
paths :: Int -> Int -> [(Int,Int)] -> [[Int]]
paths a b graph = filter (/=[]) $ paths' a b graph []

paths' :: Int -> Int -> [(Int,Int)] -> [Int] -> [[Int]]
paths' a b graph history
    | a == b && history /= []  = [history ++ [b]] -- modified from problem81
    | a `elem` history = []
    | otherwise        = concat $ [paths' adj b graph (history++[a]) | adj <- dadjs a graph]

dadjs :: Int -> [(Int,Int)] -> [Int]
dadjs _ [] = []
dadjs node (x:xs)
    | fst x == node = (snd x):(dadjs node xs)
    | otherwise     = dadjs node xs
