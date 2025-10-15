-- aprox :: (Integral a, Fractional b) => a -> b
aprox n = sum (map (\x -> 1/fact x) [0..n])

aprox' n = sum xs 
        where
            xs = [1/fact x | x <- [0..n]]
            
fact k = product [1..k]

aprox'' n = sum (terms n)
    where
        terms n = map rep [0..n]
        rep k = 1 / fact k
        fact x = product [1..x]