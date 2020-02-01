{-
Problem 55
(**) Construct completely balanced binary trees

In a completely balanced binary tree, the following property holds for every node: The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, which means their difference is not greater than one.

Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes. The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all nodes of the tree.
-}
import Data.List

main = do print $ cbalTree 4

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

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
