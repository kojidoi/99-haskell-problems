{-
Problem 70C
(*) Count the nodes of a multiway tree.

Example in Haskell:

λ> nnodes tree2
2
-}

data Tree a = Node a [Tree a]
              deriving (Eq, Show)

tree2 = Node 'a' [Node 'b' []]

main = do print $ nnodes tree2

nnodes :: Tree a -> Int
-- nnodes (Node _ []) = 1
nnodes (Node _ ts) = 1 + sum [nnodes t | t <- ts]
