isolate  [] _ = ([], [])
isolate (x:xs) n = if x /= n then (x:l, r) else (l, x:r)
            where
                (l, r) = isolate xs n