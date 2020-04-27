{-
Problem 83
(**) Construct all spanning trees

Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. With this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate, use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!

Example in Haskell:

λ> length $ spanningTree k4
16
-}

import Control.Monad

main = do print $ length $ spanningTree ([1,2,3,4], [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])

spanningTree :: ([Int],[(Int,Int)]) -> [[(Int,Int)]]
spanningTree (nodes, edges) = [es | es <- allGraphs edges, allReachable (nodes, es), closedPaths (nodes, es) == []]

allGraphs :: [(Int,Int)] -> [[(Int,Int)]]
allGraphs edges = [map fst $ filter (snd) (zip edges mask) | mask <- masks]
    where masks = replicateM (length edges) [True,False]

allReachable :: ([Int],[(Int,Int)]) -> Bool
allReachable (nodes, edges) = all (/=[]) $ allReachable' (nodes, edges)

allReachable' :: ([Int],[(Int,Int)]) -> [[[Int]]]
allReachable' (nodes, edges) = [paths n1 n2 edges | n1 <- nodes, n2 <- nodes]

closedPaths :: ([Int],[(Int,Int)]) -> [[[Int]]]
closedPaths (nodes, edges) = filter (/=[]) $ [cycle' n edges | n <- nodes]

--
cycle' :: Int -> [(Int,Int)] -> [[Int]]
cycle' a graph = paths2 a a graph

paths :: Int -> Int -> [(Int,Int)] -> [[Int]]
paths a b graph = filter (/=[]) $ paths' a b graph []

paths' :: Int -> Int -> [(Int,Int)] -> [Int] -> [[Int]]
paths' a b graph history
    | a == b           = [history ++ [b]]
    | a `elem` history = []
    | otherwise        = concat $ [paths' adj b graph (history++[a]) | adj <- adjs a graph]

paths2 :: Int -> Int -> [(Int,Int)] -> [[Int]]
paths2 a b graph = filter (/=[]) $ paths2' a b graph []

paths2' :: Int -> Int -> [(Int,Int)] -> [Int] -> [[Int]]
paths2' a b graph history
    | a == b && history /= [] && (length history) /= 2  = [history ++ [b]] -- modified condition
    | a `elem` history = []
    | otherwise        = concat $ [paths2' adj b graph (history++[a]) | adj <- adjs a graph]

adjs :: Eq a => a -> [(a,a)] -> [a]
adjs _ [] = []
adjs node (x:xs)
    | a == node = b:(adjs node xs)
    | b == node = a:(adjs node xs)
    | otherwise = adjs node xs
    where (a,b) = x

dadjs :: Eq a => a -> [(a,a)] -> [a]
dadjs _ [] = []
dadjs node (x:xs)
    | a == node = b:(dadjs node xs)
    | otherwise = dadjs node xs
    where (a,b) = x
