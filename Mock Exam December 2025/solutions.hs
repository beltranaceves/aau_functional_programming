-- Name: Beltran Aceves Gil
-- AAU mail address: baceve25@student.aau.dk
-- Study number:


-- PROBLEM 1

-- 1.1
-- remove :: (Num b) => [a] -> b -> [a]
-- remove is a polymorphic function.
-- It has parametric polymorphism due to the use of unconstrained type variables ([a])
-- It also has ad-hoc polymorphism due to the use of constrained type variables in type classes, b must be Num

remove [] _ = []
remove (x:xs) 1 = xs 
remove (x:xs) k = x : remove xs (k - 1)


-- Base case 01:
-- remove [17,42,484000,9400] 2
-- > [17, 484000, 9400]
-- Base case 02:
-- remove "mango" 7
-- > "mango"
-- Test case 01: valid input
-- remove [1..5] 3
-- > [1, 2, 4, 5]
-- Test case 02: invalid k, k < 1
-- remove [1..5] 0
-- > [1, 2, 3, 4, 5]
-- Test case 03: invalid k, k > length xs
-- remove [1..5] 10
-- > [1, 2, 3, 4, 5]

-- 1.2
-- removals :: [a] -> [[a]]
-- This function is polymorphic. It has parametric polymorphism due to the use of unconstrained type variables
removals xs = map f iterator
            where
                f = remove xs
                iterator = [1..(length xs)]

-- Base case 01:
-- removals [1, 2, 3, 4]
-- > [[2, 3, 4], [1, 3, 4], [1, 2, 3]]
-- Test case 01: empty list
-- removals []
-- > []
-- Test case 02: non-numeric list, for polymorphism
-- removals "test"
-- > ["est","tst","tet","tes"]
-- Test case 03: 
-- removals [("ini", "mini"), ("maini", "mou") ]
-- > [[("maini","mou")],[("ini","mini")]]


-- PROBLEM 2

-- 2.1
data Person = P String Bool deriving Show
--   Person =   Name   famous
data Family = Single Person [Family] | Couple Person Person [Family] deriving Show
--   Family =               Children                        Children
data Dynasty = Head Family deriving Show

-- The dynasty, families and persons are built bottom up
-- that's why I start with the deepmost leaf

-- Trevor as a Person -> Single family no children
pTrevor = P "Trevor" True
fsTrevor = Single pTrevor []

-- Archie as Person -> Single family with Trevor's family as children
pArchie = P "Archie" True
fsArchie = Single pArchie [fsTrevor]

-- Lillibeth and Kate as Persons -> Couple Family with no children
pLillibeth = P "Lillibeth" True
pKate = P "Kate" True
fcLillibethKate = Couple pLillibeth pKate []

-- Harry as a Person -> Single Family with no children
pHarry = P "Harry" True
fsHarry = Single pHarry []
-- Loreen as a Person -> Single Family with no children
pLoreen = P "Loreen" True
fsLoreen = Single pLoreen []
-- Alison as a Person -> Single Family with no children
pAlison = P "Alison" True
fsAlison = Single pAlison []

-- Ted and Alice as Persons -> Couple Family with Archie's family and Lillibeth/Kate's family as children
pTed = P "Ted" True
pAlice = P "Alice" True
fcTedAlice = Couple pTed pAlice [fsArchie, fcLillibethKate]

-- Joe and Donald as Persons -> Couple Family with Harry's, Loreen's and Alison's families as children
pJoe = P "Joe" True
pDonald = P "Donald" True
fcJoeDonal = Couple pJoe pDonald [fsHarry, fsLoreen, fsAlison]

-- Bob and Carol as Persons -> Couple Family with Ted/Alice's and Joe/Donald's families as childrem
pBob = P "Bob" False
pCarol = P "Carol" False
fcBobCarol = Couple pBob pCarol [fcTedAlice, fcJoeDonal]

-- Bob/Carol's Family as the Head of the Dynasty
dynastyT = Head fcBobCarol
{-
Head (Couple (P "Bob" False) (P "Carol" False) [Couple (P "Ted" True) (P "Alice" True) [Single (P "Archie" True) [Single (P "Trevor" True) []],Couple (P "Lillibeth" True) (P "Kate" True) []],Couple (P "Joe" True) (P "Donald" True) [Single (P "Harry" True) [],Single (P "Loreen" True) [],Single (P "Alison" True) []]])
-}

pJytte = P "Jytte" True
pBente = P "Bente" False
fcJytteBente = Couple pJytte pBente []

pBirger = P "Birger" False
fsBirger = Single pBirger []

pAnna = P "Anna" False
fsAnna = Single pAnna []

pGertrud = P "Gertrud" False
fsGertrud = Single pGertrud []

pVerner = P "Verner" False
fsVerner = Single pVerner []

pViggo = P "Viggo" True
pMads = P "Mads" False
fcViggoMads = Couple pViggo pMads [fsAnna, fsBirger]

pPeter = P "Peter" True
pSusanne = P "Susanne" True
fcPeterSusanne = Couple pPeter pSusanne [fsGertrud, fsVerner]

pJohn = P "John" False
pAlice2 = P "Alice2" True
fcJohnAlice = Couple pJohn pAlice2 [fcPeterSusanne, fcViggoMads]

dynastyN = Head fcJohnAlice
{-
Head (Couple (P "John" False) (P "Alice2" True) [Couple (P "Peter" True) (P "Susanne" True) [Single (P "Gertrud" False) [],Single (P "Verner" False) []],Couple (P "Viggo" True) (P "Mads" False) [Single (P "Anna" False) [],Single (P "Birger" False) []]])
-}


-- 2.2
-- famous :: Family -> Bool
famous (Single (P name fm) children) = fm
famous (Couple (P name1 fm1) (P name2 fm2) children) = fm1 && fm2

-- modern :: Dynasty -> Bool
modern (Head fam) | not (famous fam) = False
                  | otherwise = True
modernAux fam family | fam && not (famous family) = False
                     | otherwise = foldAnd (map (modernAux (famous family)) chl)
                     where
                        chl = familyChildren family
                        foldAnd = foldr (&&) True
                        familyChildren (Single p children) = children
                        familyChildren (Couple p1 p2 children) = children
-- PROBLEM 3

-- 3.1
-- dwindle :: [a] -> [a]
-- This function is polymorphic. It has parametric polymorphism due to the use of unconstrained type variables
dwindle [] = [[]]
dwindle (x:xs) = (x:xs) : dwindle xs 
-- 3.2
dwindle' xs = [drop x xs | x <- [0..length xs]]
-- 3.3
dwindle'' xs = foldr p v s
            where
                p = (:) . \x -> drop x xs
                v = []
                s = [0..length xs]
-- Test cases:
-- Base case 01:
-- dwindle []
-- > [[]]
-- dwindle []
-- > [[]]
-- dwindle' []
-- > [[]]
-- dwindle'' []
-- > [[]]

-- Base case 02:
-- dwindle "mango"
-- > ["mango","ango","ngo","go","o",""] -- Last empty string is missing from the problem description, but not on the second example
-- dwindle' "mango"
-- > ["mango","ango","ngo","go","o",""] -- Last empty string is missing from the problem description, but not on the second example
-- dwindle'' "mango"
-- > ["mango","ango","ngo","go","o",""] -- Last empty string is missing from the problem description, but not on the second example

-- Base case 03:
-- dwindle [6, 3, 0, 1, 2, 5]
-- > [[6,3,0,1,2,5],[3,0,1,2,5],[0,1,2,5],[1,2,5],[2,5],[5],[]]
-- dwindle' [6, 3, 0, 1, 2, 5]
-- > [[6,3,0,1,2,5],[3,0,1,2,5],[0,1,2,5],[1,2,5],[2,5],[5],[]]
-- dwindle'' [6, 3, 0, 1, 2, 5]
-- > [[6,3,0,1,2,5],[3,0,1,2,5],[0,1,2,5],[1,2,5],[2,5],[5],[]]

-- These base cases cover the function pretty well, my own test cases are not very different, basically examples of polymorphism

-- Test case 01: 
-- dwindle [True, False, False, True]
-- > [[True,False,False,True],[False,False,True],[False,True],[True],[]]
-- dwindle' [True, False, False, True]
-- > [[True,False,False,True],[False,False,True],[False,True],[True],[]]
-- dwindle'' [True, False, False, True]
-- > [[True,False,False,True],[False,False,True],[False,True],[True],[]]

-- Test case 02:
-- dwindle [("ini"), ("mini"), ("maini"), ("mou")]
-- >[["ini","mini","maini","mou"],["mini","maini","mou"],["maini","mou"],["mou"],[]]
-- dwindle' [("ini"), ("mini"), ("maini"), ("mou")]
-- >[["ini","mini","maini","mou"],["mini","maini","mou"],["maini","mou"],["mou"],[]]
-- dwindle'' [("ini"), ("mini"), ("maini"), ("mou")]
-- >[["ini","mini","maini","mou"],["mini","maini","mou"],["maini","mou"],["mou"],[]]

-- Test case 03:
-- dwindle [[]]
-- > [[[]],[]]
-- dwindle' [[]]
-- > [[[]],[]]
-- dwindle'' [[]]
-- > [[[]],[]]


-- PROBLEM 4
data Status a = Fresh a | Used a deriving Show

instance Functor Status where
    -- fmap :: Functor f => (a -> b) -> f a -> f b
    fmap f (Fresh x) = Fresh (f x)
    fmap f (Used x) = Used (f x)
-- 4.1
instance Applicative Status where
    -- pure :: Functor f => a -> f a
    pure = Fresh
    
    -- (<*>) :: f (a -> b) ->f a-> f b
    (Fresh f) <*> x = fmap f x
    (Used f) <*> x = fmap f x

-- 4.2
instance Monad Status where
    -- (>>=) :: Functor f => f a -> (a -> f b) -> f b
    (Fresh x) >>= f = f x
    (Used x) >>= f = f x
-- 4.3

-- minimise :: Ord b => Status b -> Status b -> Status b
minimise a b = do
            realA <- a
            realB <- b
            return (min realA realB)

-- Base case 01:
-- minimise (Fresh 4) (Fresh 8)
-- > Fresh 4
-- Base case 02:
-- minimise (Used "ab") (Used "aa")
-- > Fresh "aa"
-- Test case 01:
-- minimise (Used "010") (Fresh "1")
-- > Fresh "010"
-- Test case 02:
-- minimise (Fresh 'x') (Fresh 'z')
-- > Fresh 'x'
-- Test case 03:
-- minimise (Used 2.0) (Used 1.0)
-- > Fresh 1.0


-- PROBLEM 5
--------------------------
-- 5.1
-- funcA :: Num a => Bool -> a -> (a, a)
-- This functionn is polymorphic. It has ad-hoc polymorphism due to the use of constrained type variables in type classes (a must live in Num)
funcA True n = (n, n)
--------------------------
-- 5.2
-- funcB :: a1 -> a2 -> (a2, a2) -> (a1, [a2])
-- This function is polymorphic. It has parametric polymorphism due to the use of unconstrained type variables (a1 and a2)
funcB x y (l, r) = (x, y:r:[l]) 
--------------------------
-- 5.3
-- exprC :: Num a => Maybe [a -> a]
-- This expression is polymorphic. It has ad-hoc polymorphism due to the use of constrained type variables in type classes (a must live in Num)
exprC = Just [\x -> x + x]
--------------------------
-- 5.4
-- funcE :: p1 -> p2 -> [a]
funcE x y = []
--------------------------
-- 5.5
-- funcD :: Bool -> String
-- This function is monomorphic, it is only defined for specific types
funcD True = "False"

-- PROBLEM 6

-- 6.1
-- squares :: Num a => [a]
-- Function is ad-hoc polymorphic
squares = square 1
        where
            square i = i^2 : square (i+1)

-- 6.2
-- squares' :: Num a => [a]
squares' = map (^2) [1..]

-- 6.3
v = (\y -> (tail [1 / (3 - y), 7.9, 11.3])) (1+2)
-- The expression will be evaluated using outtermost order

-- Step 01: the lambda and first parameter (1+2) are considered.
-- The only way to reduce the expression is to run the lambda f, which uses the first parameter.
-- Before using the parameter, we evaluate it to reduce it
v1 = (\y -> (tail [1 / (3 - y), 7.9, 11.3])) (3)

-- Step 02: the expression is reduced by applying the lambda f to the parameter 3
-- which means to rebuild the expression without the lambda f by replacing all instances of y with 3
v2 = tail [1 / (3 - (3)), 7.9, 11.3]

-- Step 03: we now only have a function and can only be reduced by applying it to the input
-- since tail doesn't use the first element of the list, it never calls the division by 0 
v3 = [7.9, 11.3]

-- We test partial correctness by checking the expressions are equivalen
-- v == v1
-- > True
-- v1 == v2
-- > True
-- v2 == v3
-- > True
