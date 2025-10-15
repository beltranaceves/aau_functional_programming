-- dbs :: (Eq a, Num a) => [(a, a)] -> [(a, a)]
dbs = filter (\(a, b) -> a*2 == b)

dbs' = foldr p v
        where
            p = \(a, b) xs -> if a*2 == b then (a, b):xs else xs
            v = []