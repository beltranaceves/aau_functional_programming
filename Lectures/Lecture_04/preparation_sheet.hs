{- Exercise 01:
Define, using pattern matching and without using the length function, a function onlytwo that tells
us if a list has precisely two elements – in which case it must return True – or not, in which case it
must return False. What is the type of onlytwo?
-}
onlytwo :: [a] -> Bool
onlytwo [_, _] = True
onlytwo _ = False

{- Exercise 02:
The dot product of two pairs of numbers (a, b) and (c, d) is the number a · c + b · d. Define, using
list comprehension, a function alldots that takes two lists of pairs of numbers and returns all the
possible dot products of every pair from the first list and every pair from the second list. Find two
good test case for testing your function definition and use them to test your code. What is the type
of alldots ?
-}

alldots :: Num a => [(a, a)] -> [(a, a)] -> [a]
alldots xst yst = [ a * c + b * d | (a, b) <- xst, (c, d) <- yst ]

{- Test cases:

> a = [(1, 0), (0, 1)]
> b = [(0, 1), (1, 0)]
> alldots a b
> [0, 1, 1, 0]

> c = [(1, 0)]
> d = [(1, 0)]
> alldots c d
> [1]

> a = [(1, 2), (3, 4)]
> b = [(1, 0), (0, 1)]
> alldots a b
> [1, 2, 3, 4]
-}







{-
incrementedTuples :: Num a => [(a, a)]
incrementedTuples = [(x, y) | x <- [0..], y <- [x..x+1]]

> alldots (take 5 incrementedTuples) (take 5 incrementedTuples)
> [0,0,0,0,0,0,1,1,2,2,0,1,2,3,4,0,2,3,5,6,0,2,4,6,8]
-} 
