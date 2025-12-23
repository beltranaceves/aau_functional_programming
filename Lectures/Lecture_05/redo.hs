{-
Exercise 01: The function reverse appears in the Haskell prelude. It will reverse a list such that 
e.g. reverse [1, 2, 3] evaluates to [3, 2, 1] .
Now it is your task to define your own version of this function, rev. First try to find out what the
type of rev should be and follow the overall approach described in Section 6.6
-}

-- rev :: [a] -> [a]
rev [] = []
rev (x:xs) = reverse xs ++ [x]

{-
Exercise 02:
A list [a1, a2, ..., an] is descending if a1 >= a2 >= ... >= an.
Write a function descending that will return True if a list is descending and False otherwise.
As examples, descending [6, 5, 5, 1] should return True and descending ["plip", "pli", "ppp"] should
return False. What is its type?
-}

-- descending :: (Ord a) => [a] -> Bool
descending [a, b] = a >= b 
descending (x:y:xs) = x >= y && descending (y:xs)


{-
Exercise 03: The function isolate takes a list l and an element x and returns a pair of two new lists (l1, l2).
The first list l1 is a list that contains all elements in l that are not equal to x. The second list l2
is a list that contains all ocurrences of x in l:
- isolate [4, 5, 4, 6, 7, 4] 4 evaluates to ([5, 6, 7], [4, 4, 4])
- isolate ['g', 'a', 'k', 'a'] 'a' evaluates to (['g', 'k'], ['a', 'a'])

Define isolate in Haskell without using fst, snd, head or tail. What should the type of isolate be?
-}

-- isolate :: Eq a => [a] -> a -> ([a], [a])
isolate [x] k | x == k = ([], [x])
              | otherwise = ([x], [])
              
isolate (x:xs) k | x == k = (different, x:equal)
                 | otherwise = (x:different, equal)
                where
                    (different, equal) = isolate xs k


isolate' xs k = (different, equal)
                where
                    different = [x | x <- xs, x /= k]
                    equal = [x | x <- xs, x == k]

isolate'' xs k = foldr p v s
            where
                p x (different, equal) = if x == k then (different, x:equal) else (x:different, equal)
                v = ([], [])
                s = xs

{-
Exercise 04: The function wrapup is a function that takes a list and returns a list of lists.
Each list in this list contains the successive elements from the original list that are identical.
For instance wrapup [1, 1, 1, 2, 3, 3, 2] should give us the list [[1, 1, 1], [2], [3, 3], [2]] and
...
Define wrapup in Haskell using recusrion but without using fst, snd, head or tail
-}

-- wrapup :: [a] -> [[a]]
-- wrapup [] = [[]]
wrapup [x] = [[x]]
wrapup (x:xs) = if x == y then (x:y:ks):ys else [x]:acum 
        where
            acum = wrapup xs
            ((y:ks):ys) = acum

{-
Exercise 05: A former minister of science and education has decided to get a university degree
and is not trying to define a Haskell function triples that takes a list of tuples (each tuple has exacly 3 elements)
and converts that list of tuples into a tuple of lists.
triples [(1, 2, 3), (4, 5, 6), (7, 8, 9)] should produce ([1, 4, 7], [2, 5, 8], [3, 6, 9])
-}

-- triples :: [(a, b, c)] -> ([a], [b], [c])
triples [(a, b, c)] = ([a], [b], [c])
triples ((a, b, c):xs) = (a:l1, b:l2, c:l3)
                where
                    (l1, l2, l3) = triples xs


{-
Extra problem A: The function rle is a function that, when given a list xs, produces a list of pairs
of elements of xs and integers. This list of pairs has its elements appear in the order that they appeared originally
and contains (x, n), if there are n successive occurences of x in the list. For instance

rle [1, 1, 1, 2, 2, 1, 3, 3] evaluates to [(1, 3), (2, 2), (1, 1), (3, 2)]
-}

-- rle :: (Num a) => [b] -> [(b, a)]

rle [x] = [(x, 1)]
rle (x:xs) = if x == y then (x, count + 1):ys else (x, 1):(y, count):ys
            where
                (y, count):ys = rle xs

{-
Extra problem B: Define a function amy that will tell us if any elements of a list satisfy a given predicate
-}

-- amy :: (a -> Bool) -> [a] -> Bool
amy predicate [a] = predicate a
amy predicate (x:xs) = predicate x || amy predicate xs


{-
Extra exercise C: Create a function that, given a string s, creates a list of paris
[(x1, f1), ..., (xk, fk)] such that if the character xi occurs a total number of fi times throughout the
list s, then the list of pairs will contain the pair (xi, fi).
-}

frequencies xs = freqs xs [] 
            where
                freqs [] memo = memo
                freqs (l:ls) memo = freqs ls (upt memo l)

upt [] x = [(x, 1)]
upt ((item, count):xs) x = if x == item 
                            then (item, count+1):xs
                            else (item,count) : upt xs x

{-
A theorem in number theory states that every non-zero real number x can be written as a continued
fraction. 

x = a_0 + (1/ (a_1 + 1 /(a_2 + 1/(...))))

For rational numbers, the a_i’s will eventually all be 0, so the continued fraction is finite; for irrational
numbers, the continued fraction will be infinite. See e.g. [1] for more.

The goal of this problem is to write a Haskell function cfrac that will, given a real number r and a
natural number n, finds the list of the first n numbers in the continued fraction expansion of r. What
should the type of cfrac be?
-}


