data Aexp = N Integer | V String | Add Aexp Aexp | Mult Aexp Aexp deriving Show

-- eval :: Aexp -> [(String, Integer)] -> Int
eval (V v) ass = look v ass
eval (N x) _ = x
eval (Mult x y) ass = x' * y'
                where x' = eval x ass
                      y' = eval y ass
eval (Add x y) ass = x' + y'
                where x' = eval x ass
                      y' = eval y ass

-- look :: Eq a => a -> [(a, b)]-> b
-- look v ((s, n):xs) | v == s = n
--                    | otherwise = look v xs
look s ass = head [v | (x, v) <- ass, s == x]

myExpr = Add (V "y") (Mult (V "x") (N 2))
myAss = [("x", 3), ("y", 4)]