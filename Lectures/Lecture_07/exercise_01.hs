data Aexp = Numeral Int | Variable String |
 Addition Aexp Aexp | Multiplication Aexp Aexp deriving Show

--  2 + (3 + (x * 9))
myExpr = Addition (Numeral 2) (Addition (Numeral 3) (Multiplication (Variable "x") (Numeral 9))) 
