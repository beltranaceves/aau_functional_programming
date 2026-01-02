{-
Problem 01:
-}

-- 01: The type of norm :: (Num a) => [a] -> a. Yes it is a parametric polymorphic function due to the use of unconstrained type variables
-- 02: 
norm [] = 0
norm (x:xs) = x^2 + norm xs

-- 03:

norm' xs = foldr p v s
        where
            p = (+) . (^2)
            v = 0
            s = xs

-- 04:
dist [] [] = 0
dist (x:xs) (y:ys) = (x-y)^2 + dist xs ys

{-
Problem 02: 
-}

-- 01: 
data Tree a = Node a [Tree a] | Leaf a
-- 02:
tA = Node 4 [l1, r1]
    where
        l1 = Node 4 [l2, r2]
        r1 = Node 12 [l3, r3]
        l2 = Leaf 7
        r2 = Leaf 9
        l3 = Leaf 12
        r3 = Leaf 17
tB = Node "papa" [l1, r1]
    where
        l1 = Node "yoyo" [l2, r2]
        r1 = Node "dada" [l3, m1, r3]
        l2 = Leaf "lala"
        r2 = Leaf "gygy"
        l3 = Leaf "nene"
        m1 = Leaf "fifi"
        r3 = Leaf "bubu"

-- 03:
-- uniform :: (Tree b) => b -> Bool
uniformK (Leaf _) k = True
-- uniformK (Node val xs) k = lengtgfsdfs




{-
Problem 03:
-}

-- fun1 :: Ord a => (a, a) -> String -> Integer
-- Ad-hoc polymorphism due to the use of constrained type variables, as a must live in Ord
fun1 (l, r) "Hehe" = if l > r then 1 else 0

-- fun2 :: Bool -> p -> p
-- parametric polymorphism due to the use of unconstrained type variables
fun2 True a = a

-- fun3 :: Show a1 => [a2] -> a1 -> IO()
-- parametric polymorphism due to the use of unconstrained type variables, like a2
-- ad-hoc polymorphism due to the use of constrained type variables, like a1 that must live in Show
fun3 (x:xs) a = print a

-- fun4 :: ((a1, a1), b) -> [a2] -> ((a1, b) -> [a3]) -> [a3]
-- parametric polymorphism due to the use of unconstrained type variables
fun4 ((a, b), c) (x:xs) f = tail res
                    where
                        p = [a, b]
                        res = f (a, c)



{-
Problem 04:
-}

readNumber :: String -> Maybe Int
readNumber s = case reads s of
                    [(n, "")] -> Just n
                    x -> Nothing

-- 01:
add' a b = do
            h <- a
            t <- b
            return (h + t)

-- 02:

readAndAdd a b = do
            res <- add' (readNumber a) (readNumber b)
            return res


{-
Problem 05:
-}

-- 01: repeatStrings :: (Num b, Eq b) => [a] -> [b] -> [a]
-- Parametric polymorphism due to the use of unconstrained type variables, like a
-- Ad-hoc polymorphism due to the use of constrained type variables like b, which must live in Num

-- 02:

repeatStrings _ [] = []
repeatStrings [] _ = []
repeatStrings (x:xs) (0:ys) = repeatStrings xs ys
repeatStrings (x:xs) (y:ys) = x : repeatStrings (x:xs) (y-1:ys)

-- 03:

repeatStrings' items counts = foldr p v s
                    where
                        p (i, c) accum = aux [1..c] i ++ accum
                        v = []
                        s = zip items counts
                        aux xs l = map (\ a -> l) xs

{-
Problem 06:
-}

-- 01:
triangles = [sum [0..x] | x <- [0..]]

-- 02:
triangles' = sumTri 1 0
        where
            sumTri n k = k : sumTri (n+1) (k+n)

-- 03:
triangles'' = foldr p v s
            where
                p n xs = auxFoldr n:xs
                v = []
                s = [0..]
                auxFoldr k = foldr (+) 0 [0..k]
