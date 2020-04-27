{-
Problem 89
(**) Bipartite graphs

Write a predicate that finds out whether a given graph is bipartite.

Example in Haskell:

λ> bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
True
λ> bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])
False
-}

import Control.Monad

main = do
    print $ bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)])
    print $ bipartite ([1,2,3,4,5],[(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])

bipartite :: ([Int], [(Int,Int)]) -> Bool
bipartite (nodes, edges) = any bipartite' $ allPatterns nodes
    where
        bipartite' :: [Int] -> Bool
        bipartite' nodes1 = all adjsOpposite nodes
            where
                adjsOpposite :: Int -> Bool
                adjsOpposite n = if n `elem` nodes1
                    then all (`notElem` nodes1) $ adjs n edges
                    else all (`elem` nodes1) $ adjs n edges

allPatterns :: [Int] -> [[Int]]
allPatterns nodes = [map fst $ filter snd (zip nodes mask) | mask <- masks]
    where masks = replicateM (length nodes) [True,False]

adjs :: Int -> [(Int,Int)] -> [Int]
adjs _ [] = []
adjs node (x:xs)
    | a == node = b:(adjs node xs)
    | b == node = a:(adjs node xs)
    | otherwise = adjs node xs
    where (a,b) = x
