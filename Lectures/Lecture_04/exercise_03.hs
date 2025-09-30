bighead :: (Ord a, Num b) => [a] -> b
bighead (x:xs) = sum [1 | xi <- xs, xi > x]

bigheadTail (x:xs) = bht x 0 xs 

bht _ count [] = count
bht target count (x:xs) 
    | x > target = bht target (count + 1) xs
    | otherwise = bht target count xs