triples :: [(a, a, a)] -> ([a], [a], [a])

triples [] = ([], [], [])
triples [(x, y , z)] = ([x], [y], [z])
triples (a:as) = (x:xs, y:ys, z:zs)
            where
                (x, y, z) = a
                (xs, ys, zs) = triples as