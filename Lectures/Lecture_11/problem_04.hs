data Aexp = Var String | Num Integer | Plus Aexp Aexp | Mult Aexp Aexp

look ass x = wraplook [ v | (y, v) <- ass, y == x]

wraplook [] = Nothing
wraplook x = Just (head x)

eval (Var x) ass = look ass x
eval (Num n) ass =  Just n
eval (Plus a1 a2) ass = do
                        x <- v1
                        y <- v2
                        return (x + y)
                        where
                            v1 = eval a1 ass
                            v2 = eval a2 ass

eval (Mult a1 a2) ass = do
                        x <- v1
                        y <- v2
                        return (x * y)
                        where
                            v1 = eval a1 ass
                            v2 = eval a2 ass
myass = [("x", 3), ("y", 4)]
