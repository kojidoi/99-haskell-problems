{-
Problem 4
(*) Find the number of elements of a list.

Example in Haskell:
λ> myLength [123, 456, 789]
3
λ> myLength "Hello, world!"
13
-}

main = do print $ myLength [123, 456, 789]
          print $ myLength "Hello, world!"

myLength :: [a] -> Int
myLength [] = 0
myLength [_] = 1
myLength (_:xs) = myLength xs + 1
