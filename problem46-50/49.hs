{-
Problem 49
(**) Gray codes.

An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,

n = 1: C(1) = ['0','1'].
n = 2: C(2) = ['00','01','11','10'].
n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
Find out the construction rules and write a predicate with the following specification:

% gray(N,C) :- C is the N-bit Gray code
Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?

Example in Haskell:

λ> gray 3
["000","001","011","010","110","111","101","100"]
-}

import Data.Bits

main = do print $ gray 3

gray :: Int -> [String]
gray 0 = []
gray n = [strWidth n $ binStr $ gray' n x | x <- [0..2^n-1]]

gray' :: Int -> Int -> Int
gray' n v = v `xor` (v `shiftR` 1)

binStr :: Int -> String
binStr 0 = "0"
binStr 1 = "1"
binStr n = binStr (n `div` 2) ++ binStr (n `mod` 2)

strWidth :: Int -> String -> String
strWidth n s = if m <= 0
               then s
               else (replicate m '0') ++ s
    where m = n - (length s)
