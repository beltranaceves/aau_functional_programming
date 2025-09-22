allbutsecond :: [a] -> [a]
allbutsecond [] = error "List is not long enough"
allbutsecond [x] = error "List is not long enough"
allbutsecond (x:xs) = [x] ++ tail xs


allbutsecond' :: [a] -> [a]
allbutsecond' [] = error "List is not long enough"
allbutsecond' [x] = error "List is not long enough"
allbutsecond' (x:xs) = [x] ++ drop 1 xs 

allbutsecond'' :: [a] -> [a]
allbutsecond'' [] = error "List is not long enough"
allbutsecond'' [x] = [x] -- Maybe more semantic to do this?
-- allbutsecond'' (x:y:xs) = [x] ++ xs
allbutsecond'' (x:_:xs) = x:xs