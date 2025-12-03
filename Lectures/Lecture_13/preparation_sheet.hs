{-
Exercise 01: 
Give two different definitions (one using recursion, one not using recursion) of a function nsonly 
that takesas input a numbernand gives us the infinite list consisting of 0n, 1n, 2n, 3n, . . .
-}

-- nsonly :: (Num a, Enum a) => a -> [a]
nsonly n = map (\x -> x*n) [0..]
gen3n = nsonly 3
-- > take 3 gen3n
-- > [0, 3, 6]

-- nsonly'' n = [(a*n) | a <- [0..]]

-- nsonly' :: Num p => p -> [p]
nsonly' n = nextN 0
        where
            nextN i = (i * n) : nextN (i + 1)

{-
Exercise 02:
Here is a definition of an expression.

plip = fst (17, f 484000)
        where   
            f x = f x + 1

What is the value of plip?  Explain!
-}

-- To understand to value of plit we need to consider outermost and innermost evaluation
-- With innermost evaluation, plip would cause an infinite recursice loop.
-- But since Haskell uses outermost evaluation, the offending second term f 484000 isn't evaluated
-- until its value is needed, leaving it as an unevaluated thunk.
-- The value of plip is 17


-- plip = snd (17, f 484000)
--         where   
--             f x = f x + 1
-- This would crash.