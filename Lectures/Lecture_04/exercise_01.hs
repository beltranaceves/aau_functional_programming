idhead :: Eq a => [(a, a)] -> Bool
idhead ((x, y):_) = x == y
idhead _ = False

-- The function has both parametric and ad hoc polymorphism due to the use of type variables and type classes