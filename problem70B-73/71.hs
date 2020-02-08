{-
Problem 71
(*) Determine the internal path length of a tree.

We define the internal path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.

Example in Haskell:

λ> ipl tree5
9
λ> ipl tree4
2
-}

data Tree a = Node a [Tree a]
              deriving (Eq, Show)

tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

main = do
    print $ ipl tree5
    print $ ipl tree4

ipl :: Tree a -> Int
ipl = sum . pathLengths

pathLengths :: Tree a -> [Int]
pathLengths (Node _ []) = [0]
pathLengths (Node _ ts) = 0 : map (+1) (concat [pathLengths t | t <- ts])
