data Tree a = Leaf a | Node (Tree a) (Tree a)

-- minmax
-- minorder

-- minmax (Ord a) => Tree a -> Tree a -> (a, a)


minmax (Leaf v) = (v, v)
minmax (Node l r) = (1, 2)
                    where
                        minL = fst minmax(l)
                        maxL = snd minmax(l)
                        -- minR = fst minmax(r)
                        -- maxR = snd minmax(r)
                        -- minT = if minL =< minR then minL else minR
                        -- maxT = if maxL >= maxR then maxL else maxR

-- minorder ()