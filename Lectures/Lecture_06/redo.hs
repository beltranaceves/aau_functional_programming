{-
Exercise 05: Here is a function. What does it do?  and why?  use the explanation at the very end of
Section 7.3 in the book to help you answer the "why"

findo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys
 

This function concatenates two lists, ys ++ xs
-}

-- findo :: [a] -> [a] -> [a]
fingo xs ys = foldr (:) xs ys


{-
Extra exercise A: The partition function generalizes the isolate function from Session 5. The partifion function
takes a predicate p and a list xs and returns the pair of lists of elements which do and do not satisfy the predicate

a) Define partition using filter
b) Define partiion using foldr

-}

-- a)

-- partition :: (a -> Bool) -> [a] -> ([a], [a])

partition' pred xs = (filter pred xs, filter notPred xs)
                    --  (filter pred xs, filter (not.pred) xs)
                    where
                        notPred x = not (pred x)
                        

partition'' pred xs = foldr p v s
                where
                    p x (t, f) = if pred x then (x:t, f) else (t, x:f)
                    v = ([], [])
                    s = xs

{-
Extra exercise B: How can we implement filter function using foldr?
-}

-- filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' pred xs = foldr p v s
                where
                    p x xs = if pred x then x:xs else xs
                    v = []
                    s = xs

{-
Extra exercise C: Use foldr to define a function remove which takes two strings as its arguments and removes
every letter from the second list that occurs in the first list. For example, remove "first" "second"
should give us "econd". First find out what the type of the function should be.

remove' :: [a] -> [a] -> [a]
-}

remove' [] ys = ys
remove' (x:xs) ys = remove' xs (foldr p v s)
            where
                p k ks = if k == x then ks else k:ks
                v = []
                s = ys

{-
Extra exercise D: The function map can be applied to any function, so we can write map map.
What is the type of map map? Figure this out without asking the Haskell interpreter - try to justify
the answer?

map :: (a -> b) -> [a] -> [b]

map map :: [(a -> b)] -> [([a] -> [b])]
-}

{-
Extra exercise E: The function min2 takes a list of numbers and return the second-smallest number of the input list
If a list contains duplicates, the second-smallest number and the smallest number can be identical; then
the function should return that number. Assume that every input list contains at least two numbers.
-}


min2 xs = snd (foldr p v s)
        where
            p x (mini, mini2) 
                | x <= mini = (x, mini)
                | x <= mini2 = (mini, x)
                | otherwise = (mini, mini2)
            v = (maximum xs, maximum xs)
            s = xs