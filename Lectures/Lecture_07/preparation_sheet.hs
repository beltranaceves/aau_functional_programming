{- Exercise 1.  
Unary numerals consist of a finite sequence of I’s followed by a Z.
The natural numberncan be represented asnsuccessiveI’s and a Z, so e.g.  4 is represented in unary notation as IIIIZ.  
The natural number 0 is represented as Z.Define  a  recursive  datatype  Unary  for  unary  numerals
and  use  your  type  definition  todefine a function unary2int of type unary2int  ::  Unary−>Integer 
that finds the natural number represented by a given number.  As an example, unary IIIIZ should give us 4
-}

data Unary = Z | I Unary deriving Show

unary2int :: Unary -> Int
unary2int Z = 0
unary2int (I y) = 1 + unary2int y

-- > a = I (I (I Z))
-- > unary2int a
-- > 3

{- Exercise 2: 2.  Use the declaration of the type Tree on page 97 to define a function  
least  
that finds the least element in a given binary tree.What should the type of  least  be?
-}

data Tree a = Leaf a | Node (Tree a) a (Tree a)

least :: Ord a => Tree a -> a
least (Leaf x) = x
-- least (Node left val right) = minimum [val, least left, least right]
least (Node left val right) = minimum [leastLeft, val, leastRight]
                                where
                                    leastLeft = least left
                                    leastRight = least right
 
-- least (Node left val right) = minimumFoldr [val, least left, least right]
-- minimumFoldr :: Ord a => [a] -> a
-- minimumFoldr (x:xs) = foldr min x xs


a = Node (Node (Node (Leaf 3) 5 (Leaf 1)) 5 (Leaf 98)) 10 (Leaf 20)
-- > least a
-- > 1
