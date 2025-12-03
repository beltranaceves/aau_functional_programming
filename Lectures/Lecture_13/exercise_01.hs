x = 1:(map (1+) x)

-- take' 0 [] = []
-- take' _ [a] = [a]
-- take' n (x:y:xs) = x:y:(take (n-1) y:xs)