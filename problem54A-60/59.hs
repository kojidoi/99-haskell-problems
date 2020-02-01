{-
Problem 59
(**) Construct height-balanced binary trees

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Construct a list of all height-balanced binary trees with the given element and the given maximum height.

Example:

?- hbal_tree(3,T).
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
etc......No
Example in Haskell:

λ> take 4 $ hbalTree 'x' 3
[Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
 Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
 Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
-}
import Data.List

main = do print $ take 4 $ hbalTree 'x' 3

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

hbalTree :: Eq a => a -> Int -> [Tree a]
hbalTree x n = filter isHbal $ allTree x $ maxNodesOfHeight n

maxNodesOfHeight :: Int -> Int
maxNodesOfHeight 1 = 1
maxNodesOfHeight n = 2^(n-1) + maxNodesOfHeight (n-1)

isHbal :: Tree a -> Bool
isHbal Empty = True
isHbal (Branch x l r) = abs (height l - height r) <= 1

allTree :: Eq a => a -> Int -> [Tree a]
allTree _ 0 = [Empty]
allTree x n = nub $ concat [addLeaf x t | t <- (allTree x (n-1))]

addLeaf :: Eq a => a -> Tree a -> [Tree a]
addLeaf x Empty = [leaf x]
addLeaf x (Branch y l r) = toLeft ++ toRight
    where toLeft  = [Branch y t r| t <- (addLeaf x l)]
          toRight = [Branch y l t| t <- (addLeaf x r)]

height :: Tree a -> Int
height Empty = 0
height (Branch x l r) = 1 + max (height l) (height r)
