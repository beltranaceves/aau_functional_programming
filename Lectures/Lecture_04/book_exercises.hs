import Data.IntMap (disjoint)
{- Exercise 1. Using library functions, define a function halve :: [a] -> ([a],[a]) that
splits an even-lengthed list into two halves. For example:
> halve [1,2,3,4,5,6]
> ([1,2,3],[4,5,6])
-}

halve :: [a] -> ([a], [a])
halve xs = (ys, zs)
        where
            ys = take (length xs `div` 2) xs
            zs = drop (length xs `div` 2) xs

{- Exercise 2. Define a function third :: [a] -> a that returns the third element in a list
that contains at least this many elements using:
a. head and tail;
b. list indexing !!;
c. pattern matching.
-}

-- third :: [a] -> a
-- a. head and tail
thirdA xs = head (tail (tail xs))
-- b. list intexig !!
thirdB xs = xs !! 2
-- c. pattern matching:
thirdC (_:_:z:_) = z

{- Exercise 3. Consider a function safetail :: [a] -> [a] that behaves in the same way
as tail except that it maps the empty list to itself rather than producing an
error. Using tail and the function null :: [a] -> Bool that decides if a/
list is empty or not, define safetail using:
a. a conditional expression;
b. guarded equations;
c. pattern matching.
-}

-- safetail :: [a] -> [a]

-- a. a conditional expression
safetailA xs = if length xs == 0 then [] else tail xs
-- b. guarded equations
safetailB xs 
    | xs == [] = []
    | otherwise = tail xs        
-- c. pattern matching

safetailC [] = []
safetailC xs = tail xs

{-Exercise 4. In a similar way to && in section 4.4, show how the disjunction operator ||
can be defined in four different ways using pattern matching.
-}

-- disjuntionA :: Bool -> Bool -> Bool
disjuntionA True _ = True
disjuntionA _ True = True
disjuntionA _ _ = False

disjointB True True = True
disjointB True False = True
disjointB False True = True
disjointB False False = False

disjointC False False = False
disjointC _ _ = True

disjointD a b
    | a == b = a
    | otherwise = True

