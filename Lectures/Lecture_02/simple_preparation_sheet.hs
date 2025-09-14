-- This is the simple program from the slides from the introduction

laengde :: (Num p) => [a] -> p

laengde [] = 0
laengde (x:l) = 1 + (laengde l)

myList = [2,3,17,9,69,484000]

data BTree = BLeaf Int | BBranch Int BTree BTree deriving Show

-- sumtree :: BTree -> Int

sumtree (BLeaf x) = x
sumtree (BBranch x t1 t2) = let v1 = sumtree t1
                                v2 = sumtree t2
                            in x + v1 + v2


myBigOak = BBranch 14 (BLeaf 13) (BLeaf 17)

-- Quicksort

qsort :: (Ord a) => [a] -> [a]

qsort [] = []
qsort (x:xs) = small ++ [x] ++ big
                 where small = qsort [a | a <- xs, a <= x]
                       big   = qsort [a | a <- xs, a > x]


{- Exercise 01: 
    Load the program simple.hs available from the Moodle section about this session. Try
    to evaluate laengde myList. 
    What do you think the result of sumtree myBigOak will be?
    I expect 14 + 13 + 17 = 44, because its trying to sum all the values in the tree.
    It does this by recursively summing the values of the left and right subtrees, and the current node.
    Try to explain why, Then check your answer by asking Haskell.
-}

{- Exercise 02: 
    Define a function second that will, when given a list, return the second element of the list
if it exists. As examples of what this function should do, we expect that 

    second [ 1 , 4 , 5 , 6 ]

will give us 4, and that

second [ ”some , ” b i z a r r e ” , ”mango ’ ’ ]

will give us ”bizarre”. Show that your definition of second works for these examples of
arguments. Then find two more examples of arguments and see what happens. Is your
function a total function?
-}

second :: [a] -> a
second [] = error "List too short"
second [x] = error "List too short"
second (x:xs) = head xs