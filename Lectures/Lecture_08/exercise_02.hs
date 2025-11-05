{-
A former minister of science and education is now trying to get a university degree and is busy
learning Haskell. The minister is trying to construct a function triples that takes a list of tuples
(each tuple has exactly 3 elements) and converts that list of tuples into a tuple of lists.

triples [(1,2,3), (4,5,6), (7,8,9)] should produce ([1,4,7], [2,5,8], [3,6,9]).

Fixed version using recursion and local declarations:
-}


-- triples :: [(a, a, a)] -> ([a], [a], [a])
triples [] = ([], [], [])
triples (t:ts) = (x:xs, y:ys, z:zs)
                        where
                            (x, y, z) = t
                            (xs, ys, zs) = triples ts

triples' xs = foldr p v
                where
                    p = (\(x, y, z) () -> (x:xs, y:ys, z:zs) 
                            let 
                                xs = []
                                ys = []
                                zs = [])
                    v = ([], [], [])