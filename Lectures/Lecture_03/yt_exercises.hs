{-
-- Exercise 01: What are the types of the following values?

> ['a', 'b', 'c'] :: [Char]
> ('a', 'b', 'c') :: (Char, Char, Char)
> [(False, '0'), (True, '1')] :: [(Bool, Char)]
> ([False, True], ['0', '1']) :: ([Bool], [Char])
> [tail, init, reverse] :: [[a] -> [a]]

-}

{-
-- Exercise 02: What are the types of the following functions?

> second xs = head (tail xs) :: [a] -> a
> swap (x, y) = (y, x) :: (a, b) -> (b, a)
> pair x y = (x, y) :: a -> b -> (a, b)
> double x = x*2 :: Num a => a -> a
> palindrome xs = reverse xs == xs :: Eq a => [a] -> Bool
> twice f x = f (f x) :: (a -> a) -> a -> a

-- Exercise 03: Check your answers using GHCI
-}
