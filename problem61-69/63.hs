{-
Problem 63
Construct a complete binary tree

A complete binary tree with height H is defined as follows:

The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
In level H, which may contain less than the maximum possible number of nodes, all the nodes are "left-adjusted". This means that in a levelorder tree traversal all internal nodes come first, the leaves come second, and empty successors (the nil's which are not really nodes!) come last.
Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.

We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order, starting at the root with number 1. For every node X with address A the following property holds: The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be used to elegantly construct a complete binary tree structure.

Write a predicate complete_binary_tree/2.

Example:

% complete_binary_tree(N,T) :- T is a complete binary tree with N nodes.
Example in Haskell:

λ> completeBinaryTree 4
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)

λ> isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
True
-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

--main = do
--    print $ completeBinaryTree 4
--    print $ isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)

levels :: Int -> Int
levels n = levels' 1 n

levels' :: Int -> Int -> Int
levels' l n = if n < 2^l then 1
                         else 1 + levels' (l+1) (n-2^l)

{-
    completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Empty
completeBinaryTree 1 = Branch 'x' Empty Empty
completeBinaryTree 2 = Branch 'x' (Branch 'x' Empty Empty) Empty(Branch 'x' Empty Empty)
completeBinaryTree 3 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
completeBinaryTree n = Branch 'x' (completeBinaryTree (n-3)) (completeBinaryTree 2**(n-3))
-}

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree _ = True
