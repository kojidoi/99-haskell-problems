{-
Problem 2
(*) Find the last but one element of a list.

(Note that the Lisp transcription of this problem is incorrect.)

Example in Haskell:
λ> myButLast [1,2,3,4]
3
λ> myButLast ['a'..'z']
'y'
-}

main = do print $ myButLast [1,2,3,4]
          print $ myButLast ['a'..'z']

myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:xs) = myButLast xs
