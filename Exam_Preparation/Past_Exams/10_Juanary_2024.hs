{-
Problem 01:
01. Define a fuction rotate which places the head of a list at the end of the tail of the list.
We expect that rotate [1, 2, 3] = [2, 3, 1]. Is it polymorphic?

02. Use recursion and the rotate function to define a function allrotates that producces all the rotations
of a list. We expect that allrotates [1, 2, 3] = [[1, 2, 3], [2, 3, 1], [3, 1, 2]]

03. Give another definition of allrotates called allrotates' that is not recursive
map or foldr, with rotate
-}
-- 01.
-- rotate :: [a] -> [a], yes parametric polymorphism, due to the use of unconstrained type variables


rotate (x:xs) = xs ++ [x]

-- 02.

-- allrotates :: [a] -> [[a]]. yes, parametric polymorph, use of unconstrained type variables
allrotates xs = rotateCount xs (length xs - 1)
            where
                rotateCount ys 0 = [ys]
                rotateCount ys counter = ys : rotateCount (rotate ys) (counter - 1)

-- 03.

allrotates' xs = reverse res
            where
                p _ (l, r) = (rotate l, l:r)
                v = (xs, [])
                s = xs
                (_, res) = foldr p v s
                
{-
Problem 02:

-}

-- 01:

data Tree a =     Labelled a (Tree a) (Tree a) 
                | Unlabelled (Tree a) (Tree a)
                | Leaf a

t1 = Unlabelled left1 right1
    where
        left1 = Unlabelled left2 right2
        right1 = Leaf 1964
        left2 = Leaf 17
        right2 = Leaf 484000

t2 = Labelled "bingo" left1 right1
    where
        left1 = Leaf "plip"
        right1 = Labelled "plop" left2 right2
        left2 = Leaf "uhu"
        right2 = Leaf "fedtmule"

isfull (Unlabelled l r) = False
isfull (Leaf x) = True
isfull (Labelled x l r) = isfull l && isfull r

-- preorder :: Tree a -> Maybe a
preorder (Unlabelled l r) = Nothing
preorder (Leaf x) = Just [x]
preorder (Labelled x l r) = do
                        mL <- preorder l
                        mR <- preorder r
                        return (x:(mL++mR))
{-
Problem 03: Define a function remove which takes two strings as its arguments
and removes every letter from the second that occurs in the first list.

-- 01 define using list comprehension
-- 02 define using recursion without list comprehension
-}

-- remove :: Eq a => [a] -> [a] -> [a]
remove xs ys = [y | y <- ys, not (containsOne xs y)]   
            where
                containsOne [] _ = False
                containsOne (x:xs) y = x == y 
                                    || containsOne xs y

remove' xs [] = []
remove' xs (y:ys) | contains (y:ys) xs = remove' xs ys
                  | otherwise = y: remove' xs ys
                  where
                    contains [] _ = False
                    contains _ [] = False
                    contains (k:ks) (p:ps) = p == k 
                            || contains (k:ks) ps 
                            || contains ks (p:ps) 
                            || contains ks ps

{-
Problem 04:
-}

newtype WrapString a = WS (a, String) deriving Show

instance Functor WrapString where
    fmap f (WS (x, s)) = WS (f x, s)

instance Applicative WrapString where
    pure a = WS (a, "")
    (WS (f, s1)) <*> (WS (x, s2)) = WS (f x, s1)

instance Monad WrapString where
    return x = WS (x, "")
    (WS (x, s)) >>= f = let (WS (y, s')) = f x in WS (y, s)

-- pairup :: WrapString a -> WrapString b -> WrapString (a, b)
pairup a b = do
    x <- a
    y <- b
    return (x, y)

x = WS (4, "horse")
y = WS (5, "plonk")

{-
Problem 05:
-}

-- (Ord a, Num a) => a -> a -> a -> (a, a)
fun1 a b c = (max a b, b + c)
-- [(Integer, p -> Char)]
exp1 = [(1, (\ _ -> 'C'))]

-- (t1 -> Bool -> t2) -> t1 -> t2
fun2 f a = f a True

-- (Num a, Enum a) => [a]
-- exp2 :: (Num a, Enum a)
exp2 = [0..2*2]


{-
Problem 06: 
-- 01:  give a recursive definition of the list naturals of natural numbers
-- 02: Use map to define...
-- 03:
-}

-- 01:
naturals = natur 1
        where
            natur x = x : natur (x + 1) 

-- 02:
facs = map fact [0..]
    where
        fact 0 = 1
        fact x = x * fact (x - 1)

-- 03: Recursive definition of infinite list of factorials using zipWith
-- The key insight: n! = n * (n-1)!
-- So: facs' = 1 : (multiply each position n with facs'[n-1])

facs' = 1 : zipWith (*) [1..] facs'