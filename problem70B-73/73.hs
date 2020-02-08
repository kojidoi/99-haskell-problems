{-
Problem 73
(**) Lisp-like tree representation.

There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language, which is used primarily for artificial intelligence problems. As such it is one of the main competitors of Prolog. In Lisp almost everything is a list, just as in Prolog everything is a term.

The following pictures show how multiway tree structures are represented in Lisp.

p73.png

Note that in the "lispy" notation a node with successors (children) in the tree is always the first element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this sequence of tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the Prolog list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.

(The Prolog example given is incorrect.)

Example in Haskell:

λ> display lisp tree1
"a"
λ> display lisp tree2
"(a b)"
λ> display lisp tree3
"(a (b c))"
λ> display lisp tree4
"(b d e)"
λ> display lisp tree5
"(a (f g) c (b d e))"
As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion is also possible.
-}

data Tree a = Node a [Tree a]
              deriving (Eq, Show)

tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

main = do
    print $ tree_ltl tree1
    print $ tree_ltl tree2
    print $ tree_ltl tree3
    print $ tree_ltl tree4
    print $ tree_ltl tree5

tree_ltl :: Tree Char -> String
tree_ltl (Node x []) = [x]
tree_ltl (Node x ts) = '(' : x : concat [' ' : tree_ltl t | t <- ts] ++ ")"
