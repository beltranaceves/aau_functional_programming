-- within :: Ord a => [a] -> (a, a) -> [a]
within xs (a, b) = filter (\x -> x >= a && x <= b) xs
-- within (1, 3) [1..10]

-- within' :: Ord a => (a, a) -> [a] -> [a]
within' (a, b) = filter (\x -> x >= a && x <= b)
-- if I try to define within as a higher order function, the input params switch places
--  within' [1..10] (1, 3)