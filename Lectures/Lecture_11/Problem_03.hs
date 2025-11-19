data W x = Bingo x deriving Show

instance Functor W where
    fmap f (Bingo x) = Bingo (f x)

instance Applicative W where
    pure = Bingo
    (Bingo g) <*> x = fmap g x 

instance Monad W where
    return = Bingo
    -- return = pure
    Bingo x >>= f = f x
{-
wrapadd x (Bingo y) = Bingo (x*y)
-}
  
wrapadd x y = do
        b <- y
        return (x*b)

wrapadd' x y = do
        a <- x
        b <- y
        return (a*b)
