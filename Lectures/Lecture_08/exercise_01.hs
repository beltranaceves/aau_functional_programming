{-
Find Haskell expressions that have the types

1. (Ord a, Num a) => a -> a-> [[Bool]] -> Bool 
2. Num a => (t -> a, t) -> a -> a
-}

-- funA (pos, val) (pos1, val2) xs = True
funA :: (Ord a, Num a) => a -> a-> [[Bool]] -> Bool
funA x y [[True]] = x + y < y + x


-- funB :: Num a => (t -> a, t) -> a -> a
funB (funT, t) a = funT t + a 

-- funC :: a -> a -> Integer
funC x y = 1
            where
                z = [x, y]
