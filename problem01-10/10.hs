{-
Problem 10
(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:
* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

Example in Haskell:
λ> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}

main = do print $ encode "aaaabccaadeeee"

encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode xs = [(length x, head x) | x <- pack xs]

_pack :: Eq a => ([a],[[a]]) -> ([a],[[a]])
_pack ([],ys) = ([],ys)
_pack (x:xs,[]) =  _pack (xs,[[x]])
_pack ([x],[[y]]) = if x == y then ([],([[y,x]])) else _pack ([],([[y],[x]]))
_pack ((x:xs),ys) = if x == (last (last ys)) then _pack (xs,(init ys)++[x:(last ys)]) else _pack (xs,ys++[[x]])

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = snd (_pack (xs,[]))
