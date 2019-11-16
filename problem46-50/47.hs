{-
Problem 47
(*) Truth tables for logical expressions (2).

Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.

Example:

* (table A B (A and (A or not B)))
true true true
true fail true
fail true fail
fail fail fail
Example in Haskell:

λ> table2 (\a b -> a `and'` (a `or'` not b))
True True True
True False True
False True False
False False False
-}

main = do table2 (\a b -> a `and'` (a `or'` not b))

infixl 4 `or'`
infixl 6 `and'`
-- "not" has fixity 9 by default

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

table2 f = do table' f True True
              table' f True False
              table' f False True
              table' f False False

table' f x y = do putStr $ (show x) ++ " " ++ (show y) ++ " " ++ (show $ f x y) ++ "\n"
