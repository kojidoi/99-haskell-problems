{-
Problem 9
(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example:
* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))

Example in Haskell:
λ> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}

main = do print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

_pack :: Eq a => ([a],[[a]]) -> ([a],[[a]])
_pack ([],ys) = ([],ys)
_pack (x:xs,[]) =  _pack (xs,[[x]])
_pack ([x],[[y]]) = if x == y then ([],([[y,x]])) else _pack ([],([[y],[x]]))
_pack ((x:xs),ys) = if x == (last (last ys)) then _pack (xs,(init ys)++[x:(last ys)]) else _pack (xs,ys++[[x]])

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = snd (_pack (xs,[]))
