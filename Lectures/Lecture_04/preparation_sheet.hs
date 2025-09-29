{-
Define, using pattern matching and without using the length function, a function onlytwo that tells
us if a list has precisely two elements – in which case it must return True – or not, in which case it
must return False. What is the type of onlytwo?
-}
onlytwo :: [a] -> Bool
onlytwo [_, _] = True
onlytwo _ = False

{-
The dot product of two pairs of numbers (a, b) and (c, d) is the number a · c + b · d. Define, using
list comprehension, a function alldots that takes two lists of pairs of numbers and returns all the
possible dot products of every pair from the first list and every pair from the second list. Find two
good test case for testing your function definition and use them to test your code. What is the type
of alldots ?
-}

alldots :: [(Int, Int)] -> [(Int, Int)] -> [Int]

alldots xs_t ys_t = [xt_1 * yt_1 + xt_2 * yt_2 | (xt_1, xt_2) <- xs_t, (yt_1, yt_2) <- ys_t]

alldots xst yst = [ a * c + b * d | (a, b) <- xst, (c, d) <- yst ]