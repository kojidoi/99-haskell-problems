{-
Problem 90
(**) Eight queens problem

This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

Hint: Represent the positions of the queens as a list of numbers 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.

Example in Haskell:

λ> length (queens 8)
92
λ> head (queens 8)
[1,5,8,6,3,7,2,4]
-}

import Data.List

main = do
    print $ length (queens 8)
    print $ head (queens 8)

queens :: Int -> [[Int]]
queens n = filter meetsRule $ allPatterns n

allPatterns :: Int -> [[Int]]
allPatterns n = permutations [0..n-1]

meetsRule :: [Int] -> Bool
meetsRule pat = not $ any (leftDiagonalOccupied pat) [1..(length pat-1)]

leftDiagonalOccupied :: [Int] -> Int -> Bool
leftDiagonalOccupied pat i = any onDiagonal [0..i-1]
    where
        onDiagonal j = dx == dy
            where
                dx = i - j
                dy = abs $ pat!!j - pat!!i
