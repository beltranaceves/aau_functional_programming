sumrows :: Num a => [[a]] -> [a]
sumrows = map sum
-- if I comment out the type definition, the type checker complains