{-
Exercise 01:
Define a Hasekll datatype Aexp for arithmetic expreswsions with addition, multiplication, numerals
and variables. The formation rules are

    E ::= n | x | E1 + E2 | E1 * E2

Assume that variables x are string and that numerals n are integers
-}

data Aexp = N Integer | Var String | Add Aexp Aexp | Multi Aexp Aexp

{-
Exercise 02: Use your Haskell datatype from the previous problem to define a function eval that can,
when given a term of type Aexp and an assignment ass of variables to numbers compute the value of the expression
-}

-- eval :: Aexp -> Integer

eval (N a) ass = a
eval (Var a) ass = lokup ass a

eval (Add a b) ass = eval a ass + eval b ass
eval (Multi a b) ass = eval a ass * eval b ass

lokup :: [(String, Integer)] -> String -> Integer
lokup ((var, val):xs) k = if var == k then val else lokup xs k

expA = Multi (Add (N 1) (N 2)) (Var "x")
assA = [("p", 2), ("x", 2)]

{-
Exercise 03: The authors of a new encyclopedia about values of type a represent their information
using a hierarchical structure where every entry in the encyclopedia is tagged with a key that is a string
and a value of type a. Entries can ave subentries

The authors of the encyclopedia asked a famous influencer to define a type Encyclopedia for
encyclopedias. The influencer wrote
-}

data Encyclopedia a = Node String a [Encyclopedia a]

t1 = Node "mango" True [Node "dingo" False [Node "plip" True [], Node "ninka" False []], Node "plop" True [], Node "plys" False [Node "boing" True []]]

t2 = Node "plonk" 1 [Node "zap" 3 [Node "minka" 8 []], Node "uhu" 4 [Node "gif" 9 []], Node "bingo" 5 []]


{-
Exercise 04: An encyclopedia is layered if it holds that all values at the same level of the encyclopedia are larger than the values in the level above.

-}

-- layered :: (Ord a) => Encyclopedia a -> Bool
layered (Node _ _ []) = True
layered (Node key val xs) = bigger && all layered xs
                        where
                            bigger = all (\(Node _ valS _) -> valS > val) xs