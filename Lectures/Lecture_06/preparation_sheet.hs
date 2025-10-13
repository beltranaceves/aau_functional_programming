{-
Exercise 1. Every letter in the lowercase English alphabet has a position. ”a” has position 1, ”c” has
position 3 and ”h” has position 8.
In Haskell, every string is a list of characters. So String is the same type as [Char].
We can define a function positions that, given a string of lowercase letters str gives us the
list of positions of the characters in str.
As an example, positions ”abba’’ gives us [1,2,2,1] . Use the higher-order functions in
Chapter 7 to define positions.
-}

-- positions :: (Integral b)=> String -> [b]
-- positions :: (Enum a, Integral b) => [a] -> [b] 
-- Questions: 
    -- Why does using Integral break the type?
    -- Why does the linter prefer this definition over one with the explicit argument?
positions :: Enum a => [a] -> [Int]
positions = map (\x -> fromEnum x - fromEnum 'a' + 1)

positions' :: Enum a => [a] -> [Int]
positions' = foldr p v
                where
                    p = (:) . (\x -> fromEnum x - fromEnum 'a' + 1)
                    -- p = (\x xs -> fromEnum x - fromEnum 'a' + 1 : xs)
                    v = []

positions'' :: Enum a => [a] -> [Int]
positions'' = map toNumber
                where
                    toNumber y = fromEnum y - fromEnum 'a' + 1

-- basefoldr xs = foldr p v xs
--                 where
--                     p = (:) . (+1)
--                     v = []

{-
Exercise 2. The function sumsq takes an integer n as its argument and returns the sum of the squares
of the first n integers. So sumsq n returns the sum
1 + . . . + n2
As an example, sumsq 4 gives us 30 and sumsq 9 gives us 285 . Use foldr to define sumsq
– and do not use map.
-}

sumsq :: (Num a, Enum a) => a -> a
sumsq n = foldr p v xs
        where
            p = (+) . (^2)
            v = 0
            xs = [1..n]