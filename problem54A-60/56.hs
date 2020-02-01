{-
Problem 56
(**) Symmetric binary trees

Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.

Example in Haskell:

λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
False
λ> symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
True
-}

main = do
    print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
    print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

symmetric :: Eq a => Tree a -> Bool
symmetric t = mirror t t

mirror :: Eq a => Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ xl xr) (Branch _ yl yr) = mirror xl yr && mirror xr yl
mirror _ _ = False
