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
                        
{-
Problem 04: A list xs = [x1, ..., xk] is a prefix of the list yx if we have that yx = xs or ys = [x1, ..., xk, y1, ..., ym],
that is, if ys consists of at least the elements of xs, possibly followed by more elements.
As an example, [3, 4, 5] is a prefix of [3, 4, 5, 6, 484000]. As another example, [] is a prefix of [True, False]

A list xs is found within the list ys if there exist lists z1 and z2 (on of both of which may be empty)
such that ys = z1 ++ xs ++ z2. As an examples, [3, 4, 5] if found within [1, 2, 3, 4, 5, 6, 484000].
As another example, [False, False] is found within [True, False, False, True]

a) Define a function prefix that will tell us if a list is a prefix on another list. Is prefix
    polymorphic? Why and how?
b) Use prefix to define a function fwin that will tell us if a list if found within another list.
    Is fwin polymorphic? Why and how?
-}

-- a)
-- prefix :: (Eq a) => [a] -> [a] -> Bool. This has ad-hoc polymorphic due to the use of constrained type variables wihin type classes
prefix [] _ = True
prefix xs [] = False
prefix (x:xs) (y:ys) | x == y = prefix xs ys
                     | otherwise = False

-- b)
-- fwin :: [a] -> [a] -> Bool. This has ad-hoc polymorphic due to the use of constrained type variables within type classes
fwin _ [] = False
fwin xs (y:ys) = prefix xs (y:ys) || fwin xs ys

{-
Problem 05: 
A list l is increasing wrt. some ordering relation < if whenever x appears earlier in l than y, then
x < y . The goal is now to define a Haskell function increasing that will take any list as argument
and tell us if it is increasing. For instance, increasing [1,2,7,484000] should return True. On the other hand,
increasing [”ged”,”abe”,”hest”] should return False.

a) What should the type of increasing be? Is the function polymorphic? Explain.
increasing :: (Ord a) => [a] -> Bool. This is ad-hoc polymorphism 
b) Define increasing using recursion.
c) Define increasing using suitable higher-order functions
-}

increasing [] = True
increasing [x] = True
increasing (x:y:xs) = x < y && increasing (y:xs)

-- TODO: relearn this
increasing' (x:xs) = foldr p v s
                    where
                        p = \(a,b) acc -> a < b && acc
                        v = True
                        s = zip (x:xs) xs 


{-
Problem 06: Let l = [x1, ..., xn] be a list of numbers. The squared norm of l is defined by

norm l = sum xi^2 from i = 1 to n

As an example, norm [1, 3, 5, 6] = 1^2 + 3^2 + 5^2 + 6^2 = 71

Your task is to define a Haskell function norm that computer the norm of any given list of numbers.

a)  what is the type of norm? Is norm a polymorphic function? If yes, explain if and why it is ad hoc or parametric.
b) Give a definition in Haskell of norm that uses recursion.
c) Give a definitnion in Haskell of norm' that uses foldr or foldl

Now consider two lists of numbers l1 = [x1, ..., xn], and l2 = [y1, ..., yn]. We assume that
l1 and l2 have the same length. The squared distance dist(l1, l2) between the lists is defined by

dist l1 l2 = sum (xi - yi)^w, from i = 1 to n

If l1 and l2 do not have the same length, the squared distance is not defined
d) Give a definition of dist in Haskel.

-}

-- a) norm :: (Num a) -> [a] -> a. This is ad-hoc polymorphism due to the use of constrained type variables using type classes
-- b)
norm [] = 0
norm (x:xs) = x^2 + norm xs


-- c)
-- norm' :: (Num a) => [a] -> a
norm' xs = foldr p v xs
        where
            p = (+) . (^2)
            v = 0

-- d)
dist [] [] = 0
dist (x:xs) (y:ys) = (x - y)^2 + dist xs ys

dist' xs ys = foldr p v s
            where
                p = (+) . (\ (x, y) -> (x - y)^2)
                v = 0
                s = zip xs ys


{-
Problem 07: 
The function isolate takes a list l and an element x and returns a pair of two new lists (l1 , l2).
The first list l1 is a list that contains all elements in l , that are not equal to x. The second list l2
is a list that contains all occurrences of x in l.

• isolate [4,5,4,6,7,4] 4 evaluates to ([5,6,7],[4,4,4]) .
• isolate [’ g ’,’ a ’,’ k ’,’ a ’] ’a’ evaluates to ([’ g ’,’ k ’], [’ a ’,’ a ’]) .

a) What should the type of isolate be?
isolate :: (Eq a) => [a] -> a -> ([a], [a])
b) Is isolate a polymorphic function? If yes, explain what forms of polymorphism are used.
If no, explain why isolate is not polymorphic.
Is is an ad-hoc polymorphic function due to the use of constrained type variables using type classes
c) Define isolate in Haskell
    i. using recursion
    ii. using list comprehension
    iii. using foldr
-}

-- C.I)
isolate [] _ = ([], [])
isolate (x:xs) y | x == y = (different, x:equal)
                 | otherwise = (x:different, equal)
                 where
                    (different, equal) = isolate xs y

-- C.II)
isolate' xs y = (different, equals)
                where
                    different = [x | x <- xs, x /= y]
                    equals = [x | x <- xs, x == y]

-- C.III)
isolate'' xs y = foldr p v xs
                where
                    p val (different, equals) = 
                        if val == y then (different, val:equals) else (val:different, equals)
                    v = ([], [])


{-
Problem 08:
A triangular number counts the number of dots arranged in an equilateral triangle. The nth
tirangular number is the number of dots in the triangular arrangement with n dots on each side,
and is equal to sum k, from k = 1 to n, that is, the sum of the n natural numbers from 1 to n.
The infinite sequence of triangular numbers, starting with the 0th triangular number, starts as follows

0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55

a) Give a definition in Haskell of the infinite list triangles of triangular numbers that uses list comprehention only
but not recursion.

b) Give a definition of the infinite list triangles' of trianguler numbers that uses recursion, but not list comprehension.

c) Give a definition of the infinite list triangles'' of the triangular numbers that uses higher-order functions.

-}

-- a)
triangles = [sum [0..x] | x <- [0..]]
            

-- b)


triangles' = nextTriangles 0 1
        where
            nextTriangles i n = i : nextTriangles (i + n) (n + 1)
-- c)

triCount 0 = 0
triCount n = n + triCount (n - 1)

triangles'' = map triCount [0..]


{-
Problem 09:
The set of perfect cubes is the set of natural numbers that are of the form n^3 for some n E N, ie. the infinite set

{1, 8, 27, 54, ...}

a) Using Haskell, define the infinite list cubes whose elements are the perfect cubes. What is the type of cubes?

b) Given a natural number n, the integral cube root of n is the greatest natural number i such that i^3 <= n.
As an example, the integral cube root of 9 is 2 since 2^3 = but 3^3 = 27.
Use list comprehension in haskell to define the function icr that for any natural number will compute its integral cube roo.
What is the type of icr?

c) Use foldr to define the function sumcubes that computes the sum of the first n cubes for any given n.
For instance, we should have that sumcubes 3 return 36.
-}

-- a)
-- cubes :: (Num a) => [a]. Ad-hoc polymorphic
cubes = map (^3) [0..]

-- b)
-- icr :: (Num a) => a -> a. Ad-hoc polymorphic
icr n = last candidates
        where
            candidates = [x | x <- [0..n], x^3 < n]


-- c)
-- sumcubes :: (Num b, Enum b) => b -> b
sumcubes n = foldr p v s
                where
                    p = (+) . (^3)
                    v = 0
                    s = [0..n]

{-
Problem 10:
Here is a small piece of Haskell code for defining nested pairs.

    data Nesting a b = S (a, b) | C (Nesting a b, Nesting a b)

a) What is the correct terminology for the Haskell concept of which Nesting is an example?
Parametric Polymorphic recursive algebraic datatype

b) What is the correct terminology for the Haskell concept of which S is an example?
S is a data constructor

c) Here is a Haskell expression
C (C( S ( True , True ) ,C( S ( True , True ) , S ( True , F a l s e ) ) ) , S ( True , F a l s e ) 
-}

-- depth :: (Num a) => Nesting b c -> a
data Nesting a b = S (a, b) | C (Nesting a b, Nesting a b)
depth (S v) = 1
depth (C (l, r)) = 1 + max depthL depthR
                where
                    depthL = depth l
                    depthR = depth r

    
{-
Problem 11:
The compression of a list is a list that counts successive elements that are repeated and returns a list
of pairs of the form (x, v), where (x, v) indicates that there are v succesive elements that are x's.
For instance, the compression of:
[1, 1, 1, 2, 1, 4, 4, 4, 1, 1, 6, 1, 6, 4, 4, 4, 4, 4]
is the list
[(1, 3), (2, 1), (1, 1), (4, 3), (1, 2), (6, 1), (1, 1), (6, 1), (4, 1)],

and the compression of [True, True, True, True, False, True, False, False]

is the list

[(True, 3), (False, 1), (True, 1), (False, 2)]

a) Using Haskell, define a function compress that computes the compression of a list. 
What is the type of compress? Is the function polymorphic? Justify your answer.

b) If we are given a list of pairs where each pair is of the form (x, v) where v is a natural number,
we can decompress the list such that for every element (x, v) we get v successive copies of
each x. The decompression of [(1,3) ,(2,1) ,(1,1) ,(4,3) ,(1,2) ,(6,1) ,(1,1) ,(6,1) ,(4,5) ] is
therefore [ 1,1,1,2,1,4,4,4,1,1,6,1,6,4,4,4,4,4] Using Haskell, define a function decompress
that computes the decompression of a list. What is the type of decompress? Is the function
polymorphic? Justify your answer.
-}

-- a)
-- compress :: (Eq a, Num b) => [a] -> [(a, b)]

compress []     = []
compress (x:xs) = store 1 x xs
  where
    store n v [] = [(v, n)]
    store n v (y:ys)
      | y == v    = store (n + 1) v ys
      | otherwise = (v, n) : store 1 y ys

sample = [1,1,1,2,1,4,4,4,1,1,6,1,6,4,4,4,4,4]

-- b)
decompress [] = []
decompress ((x, v):xs) = gen x v ++ decompress xs
                where
                    gen item 0 = []
                    gen item val = item: gen item (val - 1)


sampleD = [(1,3) ,(2,1) ,(1,1) ,(4,3) ,(1,2) ,(6,1) ,(1,1) ,(6,1) ,(4,5) ]

