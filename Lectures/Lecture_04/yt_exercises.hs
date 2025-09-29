{- Exercise 01:
A triple (x, y, z) of positive integers in called pythagorean if x^2 + y^2 = z^2
Using a list comprehension, defin a function:
-}
pyths :: Int -> [(Int, Int, Int)]

pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2+y^2 == z^2]

{- Exercise 02:
A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself.
Using a list comprehension, define a function, that returns the list of all perfect numbers up to a given limit.

-}

perfects :: Int -> [Int]

perfects n = [x | x <- [1..n], x == sum [f | f <- factors x, f /= x]]

factors :: Int -> [Int]

factors n = [x | x <- [1..n], n `mod` x == 0]

{- Exercise 03:
The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of the corresponding integers:

Sum of i = 0 to n - 1 of (xs_i * ys_i)

Using a list comprehension, define a function that returns the scalar product of two lists

-}

scalprod :: [Int] -> [Int] -> Int
scalprod xs ys = sum [x * y | (x, y) <- zip xs ys]
