{-
Problem 72
(*) Construct the bottom-up order sequence of the tree nodes.

Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the multiway tree Tree.

Example in Haskell:

λ> bottom_up tree5
"gfcdeba"
-}

data Tree a = Node a [Tree a]
              deriving (Eq, Show)

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

main = do print $ bottom_up tree5

bottom_up :: Tree a -> [a]
-- bottom_up (Node x []) = [x]
bottom_up (Node x ts) = concat [bottom_up t | t <- ts] ++ [x]
