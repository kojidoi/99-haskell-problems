{-
Problem 80
(***) Conversions
Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.

Example in Haskell:

λ> graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
-}

data Graph a = Graph [a] [(a,a)]
              deriving (Eq, Show)

data Adj a = Adj [(a,[a])]
              deriving (Eq, Show)

main = do print $ graphToAdj (Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')])

graphToAdj :: Eq a => Graph a -> Adj a
graphToAdj (Graph nodes edges) = Adj [(x, adjs x edges) | x <- nodes]

adjs :: Eq a => a -> [(a,a)] -> [a]
adjs _ [] = []
adjs node (x:xs)
    | a == node = b:(adjs node xs)
    | b == node = a:(adjs node xs)
    | otherwise = adjs node xs
    where (a,b) = x
