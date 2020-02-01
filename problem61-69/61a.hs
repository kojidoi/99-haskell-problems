{-
Collect the leaves of a binary tree in a list

A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.

Example:

% leaves(T,S) :- S is the list of all leaves of the binary tree T
Example in Haskell:

λ> leaves tree4
[4,2]
-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

main = do print $ leaves tree4

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = (leaves l) ++ (leaves r)
