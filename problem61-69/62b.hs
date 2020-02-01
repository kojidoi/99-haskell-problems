{-
Problem 62B
Collect the nodes at a given level in a list

A node of a binary tree is at level N if the path from the root to the node has length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.

Example:

% atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
Example in Haskell:

λ> atLevel tree4 2
[2,2]
-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

main = do print $ atLevel tree4 2

atLevel :: Tree a -> Int -> [a]
atLevel t 0 = []
atLevel Empty _ = []
atLevel (Branch x l r) n = if n == 1 then [x]
                                     else (atLevel l (n-1)) ++ (atLevel r (n-1))
