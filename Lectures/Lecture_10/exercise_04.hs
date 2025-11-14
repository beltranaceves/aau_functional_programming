-- prodthree :: Num a => [a] -> [a] -> [a] -> [a]
prodthree xs ys zs = pure (\x y z -> x*y*z) <*> xs <*> ys <*> zs
