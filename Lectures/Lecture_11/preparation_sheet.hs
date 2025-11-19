{-
Exercise 01:
Define a function

    tuple :: Monad m => m a -> m b -> m (a, b)

using explicit (>>=) and then again, now using do-notation. What does the function do
    in the case, where the monad is Maybe?
-}

-- tupleBind :: Monad m => m a -> m b -> m (a, b)
tupleBind ma mb = ma >>= \a -> 
                  mb >>= \b -> 
                  return(a, b)

-- tupleDo :: Monad m => m a -> m b -> m (a, b)
tupleDo ma mb = do
    a <- ma
    b <- mb
    return (a, b)
{- In the case the Monad is Maybe it either:
- returns Just (a,b) when both inputs are Just a and Just b.
- returns Nothing if either input is Nothing/fails.
-}



{-
Exercise 02:
What is the expression that uses (>>=) that is equivalent to the following do block?
(You will have to look up the definition of (>>=))

    do y <- z
        s y
        return (f y)
-}

-- alternative :: Monad m => m t -> (t -> m a) -> (t -> b) -> m b
{-
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b 
    
    return = pure
-}

-- alternative :: Monad m => m a -> (a -> m b) -> (a -> c) -> m c
alternative z s f = z >>= \y -> 
                   s y >>= \_ -> 
                   return (f y)
{-
If the Monad was Maybe:
- it first checks if z fails
- runs s on y, but doesn't use its value. If the value was Nothing, the entire expression is also Nothing
- returns f applied to y
-}

