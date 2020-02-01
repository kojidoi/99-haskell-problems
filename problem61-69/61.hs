{-
Problem 61
Count the leaves of a binary tree
A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
Example:
% count_leaves(T,N) :- the binary tree T has N leaves
Example in Haskell:
λ> countLeaves tree4
2
-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

main = do print $ countLeaves tree4

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = (countLeaves l) + (countLeaves r)
