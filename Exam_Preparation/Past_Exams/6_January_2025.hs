{-
Problem 01:
-}

-- 01: a -> b -> [(a, b)] -> [(a, b)]
fun1 a b xs = (a, b):xs

-- 02: Num a => a -> a -> (a, [a])
fun2 x y = (x, [y+y, x])

-- 03: (Ord a, Num a) => ([Char], a -> Bool)
exp1 = (['p'], \ a -> a > a + 1)

-- 04: [[Char] -> [Char]]
exp2 = [\('p':xs) -> xs]

{-
Problem 02:
-}
-- 01
data SWComponent = Primitive String Integer | Complex String Integer [SWComponent] deriving Show
-- 02
c1 = Complex "Main" 20 [l1, r1]
    where
        l1 = Complex  "Bingo" 12 [l2, m1, r2]
        r1 = Primitive "Dingo" 8
        l2 = Primitive "Plip" 5
        m1 = Primitive "Plop" 5
        r2 = Primitive "Mango" 2
    
c2 = Complex "Tralalala" 40 [l1, m1, r1]
    where
        l1 = Primitive "Boom" 10
        m1 = Complex "Boom" 20 [l2, r2]
        r1 = Complex "Boom" 10 [r3]
        l2 = Primitive "Zoom" 4
        r2 = Primitive "Zoom" 12
        r3 = Primitive "Zoom" 5
-- 03:

valid (Primitive _ root) = root > 0
valid (Complex _ root xs) = root > 0 && root == roots && all valid xs
                        where
                            roots = countRoots xs
                            countRoots [] = 0
                            countRoots ((Primitive _ root):xs) = root + countRoots xs
                            countRoots ((Complex _ root _):xs) = root + countRoots xs

{-
Problem 03:
-}

-- 01:
data Vote = Yes | No | Abstain deriving (Eq)

-- 02:
success xs = aux xs > 0
    where
        aux [] = 0
        aux (Yes:xs) = 1 + aux xs
        aux (No:xs) = -1 + aux xs
        aux (Abstain:xs) = aux xs

-- 03:
success' xs = (yes - no) > 0
        where
            -- yes = length [x | x <- xs, voteVal x == 1]
            -- no = length [x | x <- xs, voteVal x == -1]
            -- voteVal Yes = 1
            -- voteVal No = -1
            -- voteVal Abstain = 0 

            yes = sum [1 | Yes <- xs]
            no = sum [-1 | No <- xs]

-- 04:
success'' xs = res > 0
            where
                res = foldr p v s
                p Yes accum = accum + 1
                p No accum = accum - 1
                p Abstain accum = accum
                v = 0
                s = xs


{-
Problem 04:
-}

data Err a = Result a | Wrong Float deriving Show

-- 01
-- safelog :: Float -> Err Float
safelog x | x > 0 = Result (log x)
          | otherwise = Wrong x

-- 02
instance Functor Err where
    -- fmap :: (a -> b) -> Err a -> Err b
    fmap f (Result x) = Result (f x)
    fmap f (Wrong x) = Wrong x

-- 03:

instance Applicative Err where
    pure = Result

    (<*>) :: Err (a -> b) -> Err a -> Err b
    Result f <*> x = fmap f x
    _ <*> Wrong x = Wrong x
    -- Wrong f <*> _ = Wrong f
    
-- 04:

instance Monad Err where
    return = pure
    (>>=) (Result x) g = g x
    (>>=) (Wrong x) g = Wrong x

safesum a b = do
    x <- safelog a
    y <- safelog b
    return (x + y)


{-
Problem 05:
-}

-- 01:

odds = incGen 1 2
    where
        incGen n k = n:incGen (n + k) k

-- 02:
 
tup = zip odds over
    where
        over = map (\x -> 1/x) odds
        

-- 03:
plop x = plop x

quango u v = u ++ (if (length u) > 3 then "ringo" else v)

{- The call succeeds thanks to lazy evaluation.
The failure condition is that any call to the function plop results in an infinite loop
Haskell uses outermost evaluation, and in the expression the only step forward is to run the function quango
since the length of the parameter "paul" is greater than 3, the function quango does not need to use the value of the second parameter
(plop 14) and thus never evaluates it an leaves it as an unevaluated trunk
-}


{-
Problem 06:
-}

-- ft :: Integral b => [(a, b)]

-- 01:
dec [] _ = []
dec ((x, f):xs) k 
                | x /= k = (x, f) : dec xs k
                | f > 1 = ((x, f - 1):xs)
                | otherwise = xs

ft1 = [("a", 2), ("b", 1)]
lt1 = ["a", "b", "a"]

-- 02:

-- validfq :: (Integral b) => [(a, b)] -> [a] -> Bool
-- THIS IS WRONG
validfq ft xs = length (aux ft xs) == 0
        where
            aux ft [] = ft
            aux ft (x:xs) = aux (dec ft x) xs