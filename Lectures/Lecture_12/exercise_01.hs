-- frequencies :: (Num n, Eq a) => [a] -> [(a, n)]
-- frequencies [] fs = fs
-- frequencies (x:xs) fs = frequencies xs fs'
--                                 where
--                                     fs' = map (\(k, v) -> if k == x then (k, v + 1) else (k, v)) fs

frequencies [] = []
frequencies (x:xs) = upd x (frequencies xs)

upd x [] = [(x, 1)]
upd x ((y, f):ys) | x == y = (y, f + 1):ys
upd x ((y, f):ys) | x /= y = (y, f): (upd x ys)