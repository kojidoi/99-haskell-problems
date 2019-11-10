{-
Problem 27
Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.

Example:

* (group3 '(aldo beat carla david evi flip gary hugo ida))
( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
... )
b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

Example:

* (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
... )
Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".

Example in Haskell:

λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
(altogether 1260 solutions)

λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
(altogether 756 solutions)
-}

main = do print $ group3 ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]

-- 2, 3, 4

group3 :: [a] -> [[a]]
group3 [] = [[]]
group3 xs = [[xs!!i21, xs!!i22, xs!!i31, xs!!i32, xs!!i33, xs!!i41, xs!!i42, xs!!i43, xs!!i44] |
    i21<-[0..8], i22<-[0..8],
    i31<-[0..8], i32<-[0..8], i33<-[0..8],
    i41<-[0..8], i42<-[0..8], i43<-[0..8], i44<-[0..8],
    i21/=i22, i21/=i31, i21/=i32, i21/=i33, i21/=i41, i21/=i42, i21/=i43, i21/=i44,
    i22/=i31, i22/=i32, i22/=i33, i22/=i41, i22/=i42, i22/=i43, i22/=i44,
    i31/=i32, i31/=i33, i31/=i41, i31/=i42, i31/=i43, i31/=i44,
    i32/=i33, i32/=i41, i32/=i42, i32/=i43, i32/=i44,
    i33/=i41, i33/=i42, i33/=i43, i33/=i44,
    i41/=i42, i41/=i43, i41/=i44,
    i42/=i43, i42/=i44,
    i43/=i44
    ]
