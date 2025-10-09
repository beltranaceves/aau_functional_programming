-- reverse' :: [a] -> [a]
reverse' [] = []
reverse' [a, b] = [b, a]
reverse' (x:xs) = reverse' xs ++ [x]

-- This is the Prelude implementation
reverse'' xs = reverseHelper xs []
    where
        reverseHelper [] rl = rl 
        reverseHelper (x:xs) rl = reverseHelper xs (x:rl)