{- FP8 - Exercise 01:
Without looking at the standard prelude, define the following library functions using recursion:

Decide if all logical values in a list are true
and : Bool b => [Bool] -> Bool

Concatenate
-}

and' :: [Bool] -> Bool
and' [a] = a
and' (x : xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [xs] = xs
concat' (x : xs) = x ++ concat xs

{-
Select the nth element of a list
(!!!) :: (Integral b, Eq b) =>[a] -> b -> a
-}

(!!!) :: (Integral b, Eq b) => [a] -> b -> a
(!!!) (x : _) 0 = x
(!!!) (_ : xs) n = xs !!! (n - 1)

{-
Decide if a value is an element of a list
elem :: (Eq a) => a -> [a] -> Bool
-}

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)  = (a == x) || elem' a xs

