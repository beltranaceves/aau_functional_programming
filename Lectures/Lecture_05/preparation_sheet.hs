{- Exercise 01. Define the function replicate using recursion – and use pattern matching in your solution. This
function takes an integer n and and an element x and gives us a list with n elements where x has
been repeated exactly n times. As an example, replicate 3 5 should give us [5,5,5] . What should
the type of replicate be?
-}
replicate' :: (Integral n, Eq n) => n -> a -> [a]

replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x


{- Exercise 02. Define the function improve using recursion – and use pattern matching in your solution. It takes
a list xs and, if xs contains at least two elements, it gives us a list where every other element has
been removed.
As an example, improve [1,2,3,4,5,6,7] should give us [1,3,5,7] . What should the type of improve
be?
-}

improve :: [a] -> [a]

improve [] = []
improve [a] = [a]
improve (a:_:xs) = a : improve xs 
