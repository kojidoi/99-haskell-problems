{-
Problem 87
(**) Depth-first order graph traversal (alternative solution)

Write a predicate that generates a depth-first order graph traversal sequence. The starting point should be specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first order).

Example in Haskell:

λ> depthFirst ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1
[1,2,3,4,5]
-}

import Data.List

main = do print $ depthFirst ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1

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
