-- Standard library functions

{-
Selects the first element of a non-empty list
    > head [1, 2, 3, 4, 5]
    > 1

Remove the first element from a non-empty list
    > tail [1, 2, 3, 4, 5]
    > [2, 3, 4, 5]

Select the nth element of a list (counting from zero)
    > [1, 2, 3, 4, 5] !! 2

Select the first n elements of a list
    > take [1, 2, 3, 4, 5] 3
    > [1, 2, 3]

Remove the first n elements of a list
    > drop [1, 2, 3, 4, 5] 2
    > [3, 4, 5]

Calculate the length of a list
    > length [1, 2, 3, 4, 5]
    > 5

Calculate the sum of a list of numbers
    > sum [1, 2, 3, 4, 5]
    > 15

Calculate the product of a list of numbers
    > product [1, 2, 3, 4, 5]
    > 120

Append two lists
    > [1, 2, 3] ++ [4, 5]
    > [1, 2, 3, 4, 5]

Reverse a list
    > reverse [1, 2, 3, 4, 5]
    > [5, 4, 3, 2, 1]
-}

-- Exercise 01: Try out slides 2-7 and 13-16 using GHCI

-- Exercise 02: Fix the sytax errors in the program below,
-- and test your solution using GHCI
{-
N = a 'div' length xs
    where 
        a = 10
       xs = [1,2,3,4,5]
-}

n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

-- Exercise 03: Show how the library function last that selects
-- the last element of a list can be defined using the functions introduced
-- in this lecture.

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1)

-- Exercise 04: Can you think of another possible definition?

last' :: [a] -> a
last' [] = error "empty list" 
last' xs = head (reverse xs)
-- or
last''' :: [a] -> a
last''' [] = error "empty list"
last''' [x] = x
last''' (_:xs) = last''' xs

-- Exercise 05: Similarly, show how the library function init that
-- removes the last element of a list can be defined in two different ways.
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs
-- or
init'' :: [a] -> [a]
init'' [] = error "empty list"
init'' xs = reverse (tail (reverse xs))
