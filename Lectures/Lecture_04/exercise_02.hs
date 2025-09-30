-- pyt :: Int -> [(Int, Int, Int)]
pyt :: (Num c, Ord c, Enum c) => c -> [(c, c, c)]
pyt n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2, x <= y && y < z]