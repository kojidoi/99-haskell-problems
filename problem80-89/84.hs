{-
Problem 84
(**) Construct the minimal spanning tree

Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled graph. Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of the example graph to the right can be found in the file p84.dat.

Example in Haskell:

λ> prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
[(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
-}

import Control.Monad

main = do print $ prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

prim :: [Int] -> [(Int,Int,Int)] -> [(Int,Int,Int)]
prim nodes arcs = minTree $ map (edgesToArcs arcs) $ spanningTree (nodes, (arcsToEdges arcs))

arcsToEdges :: [(Int,Int,Int)] -> [(Int,Int)]
arcsToEdges xs = [(a,b) | (a,b,c) <- xs]

edgesToArcs :: [(Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int,Int)]
edgesToArcs allArcs edges = [(x,y,z) | (a,b) <- edges, (x,y,z) <- allArcs, a == x, b == y]

minTree :: [[(Int,Int,Int)]] -> [(Int,Int,Int)]
minTree [x] = x
minTree (x1:x2:xs)
    | (sumLabels x1) < (sumLabels x2) = minTree (x1:xs)
    | otherwise                       = minTree (x2:xs)

sumLabels :: [(Int,Int,Int)] -> Int
sumLabels arcs = sum [c | (a,b,c) <- arcs]

--
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
