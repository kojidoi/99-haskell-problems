{-
Problem 41
(**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
Example:
    * (goldbach-list 9 20)
    10 = 3 + 7
    12 = 5 + 7
    14 = 3 + 11
    16 = 3 + 13
    18 = 5 + 13
    20 = 3 + 17
    * (goldbach-list 1 2000 50)
    992 = 73 + 919
    1382 = 61 + 1321
    1856 = 67 + 1789
    1928 = 61 + 1867
Example in Haskell:
    λ> goldbachList 9 20
    [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
    λ> goldbachList' 4 2000 50
    [(73,919),(61,1321),(67,1789),(61,1867)]
-}

main = do
    print $ goldbachList 9 20
    print $ goldbachList' 4 2000 50

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList s e = [gn | n <- [s..e], even n, gn <- [goldbach n]]

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' s e th = filter (\gn->fst gn > th && snd gn > th) (goldbachList s e)


goldbach :: Int -> (Int,Int)
goldbach n = head [(p,q) | p<-primes, q<-primes, p+q==n] where primes = primesR 2 n

primesR :: Int -> Int -> [Int]
primesR s e = [x | x <- [s..e], isPrime x]

isPrime :: Int -> Bool
isPrime n
    | n <= 2 = True
    | otherwise = not $ any divisible [2 .. n `div `2]
        where divisible d = (n `mod` d) == 0
