midtover :: [a] -> ([a], [a])

midtover xs = (xs_first, xs_last)
                where
                    halfway = length xs `div` 2
                    xs_first = take halfway xs
                    xs_last = drop halfway xs