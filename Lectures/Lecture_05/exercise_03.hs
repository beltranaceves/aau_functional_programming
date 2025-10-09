-- isolate ::  Eq a => [a] -> a -> ([a],[a])

isolate [] _ = ([],[])
isolate (y:ys) x = if y == x then (l1,y:l2)
                   else (y:l1,l2)
                   where (l1,l2) = isolate ys x

                      -- l1 list of elements different from x
                      -- l2 list of elements equal to x
                      -- we put the recursive call in a where
                      -- we use a pattern! (l1,l2)

{-
(20 minutes) The function wrapup is a function that takes a list and returns a list of lists. Each
list in this list contains the successive elements from the original list that are identical.
For instance, wrapup [1,1,1,2,3,3,2] should give us the list [[1,1,1],[2],[3,3],[2]] and
wrapup [True,True,False,False,False ,True] should give us the list [[ True,True ],[ False , False , False
],[ True]].
-}

-- wrapup :: (Eq a) => [a] -> [[a]]


wrapup [] = [] -- ?
wrapup [a] = [[a]]
wrapup (x:y:xs) = if x == y then totalList
                  else totalList ++ [x]
                  where 
                    totalList = wrapup y:xs



wrapup' :: Eq a => [a] -> [[a]]
wrapup' [] = []
wrapup' [x] = [[x]]
wrapup' (x:y:xs) = if x == y
                then (x:l):ls
                else [x]:l:ls
                where
                    (l:ls) = wrapup' (y:xs)