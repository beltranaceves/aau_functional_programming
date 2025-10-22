data Expr = Val Int | Add Expr Expr | Mul Expr Expr

size :: Expr -> Int
size (Val _) = 1
size (Expr x y) = size x + size y 