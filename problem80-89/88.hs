{-
Problem 88
(**) Connected components (alternative solution)

Write a predicate that splits a graph into its connected components.

Example in Haskell:

λ> connectedComponents ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])
[[1,2,3,4,5][6,7]]
-}

import Data.List

main = print $ connectedComponents ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])

connectedComponents :: ([Int], [(Int,Int)]) -> [[Int]]
connectedComponents (nodes, edges) = nub [sort $ depthFirst (nodes, edges) n | n <- nodes]

--
depthFirst :: ([Int], [(Int,Int)]) -> Int -> [Int]
depthFirst (nodes, edges) node = nub $ depthFirst' (nodes, edges) node []

depthFirst' :: ([Int], [(Int,Int)]) -> Int -> [Int] -> [Int]
depthFirst' (nodes, edges) node history
    | node `elem` history = []
    | otherwise = [node] ++
        concat [depthFirst' (nodes, delete aEdge edges) aNode (history++[node]) | (aNode,aEdge) <- adjs node edges]

adjs :: Int -> [(Int,Int)] -> [(Int,(Int,Int))]
adjs _ [] = []
adjs node (x:xs)
    | a == node = (b,x):(adjs node xs)
    | b == node = (a,x):(adjs node xs)
    | otherwise = adjs node xs
    where (a,b) = x
