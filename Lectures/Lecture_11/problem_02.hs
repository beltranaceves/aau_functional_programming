data W x = Bingo x deriving Show

instance Functor W where
    fmap f (Bingo x) = Bingo (f x)

instance Applicative W where
    pure = Bingo
    (Bingo g) <*> x = fmap g x 

instance Monad W where
    return x = Bingo x
    Bingo x >>= f = f x
