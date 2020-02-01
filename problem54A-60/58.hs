{-
Problem 58
(**) Generate-and-test paradigm

Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.

Example:

* sym-cbal-trees(5,Ts).
Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
Example in Haskell:

λ> symCbalTrees 5
[Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
-}
import Data.List

main = do print $ symCbalTrees 5

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree n

----------------------------------------
cbalTree :: Int -> [Tree Char]
cbalTree n = filter (\x -> (maxDepth x - minDepth x) <= 1) $ allTree n

allTree :: Int -> [Tree Char]
allTree 0 = [Empty]
allTree n = nub $ concat [addLeaf 'x' t | t <- (allTree (n-1))]

addLeaf :: a -> Tree a -> [Tree a]
addLeaf x Empty = [leaf x]
addLeaf x (Branch y l r) = toLeft ++ toRight
    where toLeft  = [Branch y t r| t <- (addLeaf x l)]
          toRight = [Branch y l t| t <- (addLeaf x r)]

minDepth :: Tree a -> Int
minDepth Empty = 0
minDepth (Branch x l r) = 1 + min (minDepth l) (minDepth r)
          
maxDepth :: Tree a -> Int
maxDepth Empty = 0
maxDepth (Branch x l r) = 1 + max (maxDepth l) (maxDepth r)

----------------------------------------
symmetric :: Eq a => Tree a -> Bool
symmetric t = mirror t t

mirror :: Eq a => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ xl xr) (Branch _ yl yr) = mirror xl yr && mirror xr yl
mirror _ _ = False
