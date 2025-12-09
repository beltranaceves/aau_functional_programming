{-
Below are four types.  For each type, find an expression or definition that will have this particular 
typeas its most general typeif you use type inference.  
In each case, explain if the expression or definition ispolymorphic and if it is, in which way it is polymorphic.
-}
-- 1.  a−>b−>[(a, b)]−>[(a, b)]
funcA n k xs = (n, k):xs
-- 2.  Num a =>a−>a−>(a, [a])
funcB n k = (n+k, [n*k])
-- 3.  (Ord a, Num a) =>([Char], a−>Bool)
exprC = (xs, f)
        where
            (x:xs) = "exam1"
            f n = n + n < n + n
-- 4.  [[Char]−>[Char]]
exprD = [(\(x:xs) -> 'a':xs)]
