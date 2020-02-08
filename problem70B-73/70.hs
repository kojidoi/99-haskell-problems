{-
Problem 70
(**) Tree construction from a node string.

We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack to the previous level.

By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^

p70.gif

Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when the String is given. Make your predicate work in both directions.

Example in Haskell:

λ> stringToTree "afg^^c^bd^e^^^"
Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]

λ> treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
"afg^^c^bd^e^^^"
-}

data Tree a = Node a [Tree a]
              deriving (Eq, Show)

main = do
    print $ stringToTree "afg^^c^bd^e^^^"
    print $ treeToString (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])


stringToTree :: String -> Tree Char
stringToTree s = head $ snd $ s2t s

s2t :: String -> (String, [Tree Char])
s2t [] = ([], [])
s2t ('^':xs) = (xs, [])
s2t (x:y:ys) = let (s, ts)   = s2t (y:ys)
                   (s2, ts2) = s2t s
               in (s2, (Node x ts):ts2)


treeToString :: Tree Char -> String
treeToString (Node x ts) = x : concat [treeToString t | t <- ts] ++ ['^']
