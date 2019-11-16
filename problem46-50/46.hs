{-
Problem 46
(**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.

Example:

(table A B (and A (or A B)))
true true true
true fail true
fail true fail
fail fail fail
Example in Haskell:

λ> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
-}

main = do table (\a b -> (and' a (or' a b)))

not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' x y = not' $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not' $ or' x y

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

-- Implication
impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = False

equ' :: Bool -> Bool -> Bool
equ' x y = x == y

table f = do table' f True True
             table' f True False
             table' f False True
             table' f False False

table' f x y = do putStr $ (show x) ++ " " ++ (show y) ++ " " ++ (show $ f x y) ++ "\n"
