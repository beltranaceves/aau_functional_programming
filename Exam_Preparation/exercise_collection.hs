{-
Problem 01: Below are five types. For each of them, define a Haskell value (which may be a function)
that has this particular type as their most general type.

Moreover, for each of these four types also indicate if the type involves
• parametric polymorphism only
• overloading (ad hoc-polymorphism) only
• both forms of polymorphism
• no polymorphism
-}

-- Ord a => (a, a) -> String -> Integer
-- Only has ad-hoc polymorphism
funcA (l, r) count | l > r = read count :: Integer
                   | l <= r = read count :: Integer

-- Bool -> p -> p
-- Only has parametric polymorphism
funcB condition term = if condition then term else term

-- (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
-- Only has ad-hoc polymorphism
funcC xs ys (l, r) | xs == ys = if l > r then l else r
                   | otherwise = if l <= r then r else l

-- Show a1 => [a2] -> a1 -> IO()
-- Has both parametric and ad-hoc polymorphism
funcD [] x = print x

-- ((a1, a1), b) -> [a2] -> ((a1, b) -> [a3]) -> [a3]
funcE ((l1, l2), r) [] f = res
                        where
                            typeList = [l1, l2]
                            res = f (l1, r)
                            last = head res
-- Parametric polymorphism

{-
Problem 02: Here is the definition of a Hasekll function.

madras (f, x, y) = f (f x x) y

Give a curried version of madras that has type (t -> t -> t) -> t -> t -> t
-}

madras f x y = f fx y
                where 
                    fx = f x x
                    
-- madras1 (\ x y -> x + 1) 1
-- madras1 2
-- > 3

{-
Problem 03: A palindrome is a string that is the same written forwards and backwards such as "Otto"
or "Madam".

The goal of this problem is to write a Haskell function ispalindrom that will determina if a string
of characters is a palindrome

a) First figure out the type of ispalindrome without using the Haskell system. 
    Is the function polymorphic? Why? How?
b) Now give two different definitions of the function, one that uses the reverse function and one that does not.
-}

-- ispalindrome :: String -> Bool. This is not polymorphic, does not use type variables
-- which is equivalent to
-- ispalindrome :: [Char] -> Bool. This is not polymorphic, does not use type variables
-- ispalindrome :: (Eq a) -> [a] -> Bool. This is ad-hoc polymorphism, uses type variables but must belong to the type class Eq

ispalindrome term = term == reverse term

ispalindrome' [] = True
ispalindrome' [_] = True
ispalindrome' term = first == lst && ispalindrome' term'
                    where
                        first = head term
                        lst = last term
                        term' = init (tail term)