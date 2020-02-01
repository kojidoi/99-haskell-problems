{-
Problem 57
(**) Binary search trees (dictionaries)

Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search tree from a list of integer numbers.

Example:

* construct([3,2,5,7,1],T).
T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
Then use this predicate to test the solution of the problem P56.

Example:

* test-symmetric([5,3,18,1,4,12,21]).
Yes
* test-symmetric([3,2,5,7,4]).
No
Example in Haskell:

λ> construct [3, 2, 5, 7, 1]
Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
λ> symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
True
λ> symmetric . construct $ [3, 2, 5, 7, 1]
True
-}

main = do
    print $ construct [3, 2, 5, 7, 1]
    print $ symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
    print $ symmetric . construct $ [3, 2, 5, 7, 1]

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

construct :: Ord a => [a] -> Tree a
construct [] = Empty
construct xs = add xs Empty

add :: Ord a => [a] -> Tree a -> Tree a
add [] t = t
add (x:xs) Empty = add xs (leaf x)
add (x:xs) (Branch y l r) = add xs (if x < y then (Branch y (add [x] l) r)
                                             else (Branch y l (add [x] r)))

----------------------------------------
symmetric :: Eq a => Tree a -> Bool
symmetric t = mirror t t

mirror :: Eq a => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ xl xr) (Branch _ yl yr) = mirror xl yr && mirror xr yl
mirror _ _ = False
