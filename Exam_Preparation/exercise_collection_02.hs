{-
Problem 01:
-}

-- a) Ord a => (a, a) -> String -> Integer
fun1 (l, r) "HAHA" = if l < r then 1 else 2

-- b) Bool -> p -> p
fun2 True x = x

-- c) (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
fun3 a b (c, d) = if a == b then maximum [c, d] else c

-- d) Show a1 => [a2] -> a1 -> IO ()
fun4 (x:xs) k = print k

-- e) ((a1, a1), b) -> [a2] -> ((a1, b) -> [a3] -> [a3])
fun5 ((l, r), p) (y:ys) f = res
                where
                    l1 = [l, r]
                    res = f (l, p) 
                    last = head res
        

{-
Proble 02: Here is the definition of a haskell function:

- madras (f, x, y) = f (f x x) y

Give a curried version of madras that has type (t -> t -> t) -> t -> t -> t
-} 

-- madras :: (t -> t -> t) -> t -> t -> t
madras f x y = f (f x x) y

a = madras (\x y -> x + 1)

{-
Problem 03: A palindrome is a string that is the same written forwards and backwards such as "Otto" or "Madam".
The goal of this problem is to write a Haskell function ispalindrome that will determina if a string is a palindrome

- a) First figure out the type?
- b) one def using reverse one not

-}

-- ispalindrome :: (Eq a) => [a] -> Bool

ispalindrome xs = xs == reverse xs

ispalindrome' xs