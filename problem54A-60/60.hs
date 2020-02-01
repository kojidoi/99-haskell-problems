{-
Problem 60
(**) Construct height-balanced binary trees with a given number of nodes

Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?

Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult. Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of nodes in a height-balanced binary tree of height H.

On the other hand, we might ask: what is the maximum height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.

Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes. Find out how many height-balanced trees exist for N = 15.

Example in Prolog:

?- count_hbal_trees(15,C).
C = 1553
Example in Haskell:

λ> length $ hbalTreeNodes 'x' 15
1553
λ> map (hbalTreeNodes 'x') [0..3]
[[Empty],
 [Branch 'x' Empty Empty],
 [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
 [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
 -}
import Data.List

main = do
    print $ length $ hbalTreeNodes 'x' 15
    print $ map (hbalTreeNodes 'x') [0..3]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

hbalTreeNodes :: Eq a => a -> Int -> [Tree a]
hbalTreeNodes x n = filter isHbal $ allTree x n

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
