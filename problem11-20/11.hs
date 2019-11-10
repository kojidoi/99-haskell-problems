{-
Problem 11
(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:

* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))
Example in Haskell:

λ> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

-- main = do print $ encodeModified "aaaabccaadeeee"
main = do let xs = [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
          print $ encodeModified "aaaabccaadeeee" == xs

data Elem a = Single a | Multiple Int a

encodeModified :: Eq a => [a] -> [Elem a]
encodeModified xs = [if (fst x) == 1 then Single (snd x) else Multiple (fst x) (snd x) | x <- encode xs]

-- from problem 10
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
