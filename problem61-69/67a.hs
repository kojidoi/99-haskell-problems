{-
Problem 67A
A string representation of binary trees

Somebody represents binary trees as strings of the following type:

a(b(d,e),c(,f(g,)))
a) Write a Prolog predicate which generates this string representation, if the tree is given as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e. given the string representation, construct the tree in the usual form. Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.

Example in Prolog

?- tree_to_string(t(x,t(y,nil,nil),t(a,nil,t(b,nil,nil))),S).
S = 'x(y,a(,b))'
?- string_to_tree('x(y,a(,b))',T).
T = t(x, t(y, nil, nil), t(a, nil, t(b, nil, nil)))
Example in Haskell:

λ> stringToTree "x(y,a(,b))" >>= print
Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
λ> let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)
True
-}
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

--main = do
--    stringToTree "x(y,a(,b))" >>= print
--    let t = cbtFromList ['a'..'z'] in stringToTree (treeToString t) >>= print . (== t)

stringToTree :: String -> Tree Char
stringToTree "" = Empty
stringToTree [x] = Branch x Empty Empty
stringToTree (x:xs) = Branch x (stringToTree $ fst $ tokens xs) (stringToTree $ snd $ tokens xs)
    where tokens xs = if xs!!1 == ',' then ([], (drop 2 $ init xs))
                                      else ([(xs!!1)], (drop 3 $ init xs)) -- uncompleted

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"
