{-
Exercise 01:

Earlier in the course, we saw how one can declare an algebraic datatype for onions. Here it is again.

    data Onion a = Core a | Layer Onion deriving Show

We saw how to define Onion to be a functor. Now define it to be an applicative functor and a Monad

-}
data Onion a = Core a | Layer (Onion a) deriving Show

instance Functor Onion where
    -- fmap :: (a -> b) -> Onion a -> Onion b
    fmap f (Layer o) = Layer (fmap f o)
    fmap f (Core c) = Core (f c)

instance Applicative Onion where
    -- pure :: a -> Onion a
    pure = Core
    -- (<*>) :: Onion (a -> b) -> Onion a -> Onion b
    (Layer l) <*> x = l <*> x
    (Core g) <*> x = fmap g x

instance Monad Onion where
    return = pure
    Core x >>= f = f x
    Layer x >>= f = Layer (x >>= f)

onionPair a b = do 
                cA <- a
                cB <- b
                return (cA, cB)

onionA = Layer (Layer (Core 1))
onionB = Layer (Core 2)

-- > onionPair onionA onionB
-- > Layer (Layer (Layer (Core (1, 2))))
-- Current bind behaviour is that peeled onion layer stack on return
-- not sure if correct


{-
Exercise 02:
What does this piece of code do? Which monad is involved? Explain
-}

-- plop :: (Monad m) => (a -> m b) -> [a] -> m [b]
plop f [] = return []
plop f (x:xs) = do
                    y <- f x
                    ys <- plop f xs
                    return (y:ys)
                    
-- Given the code, f must be a function that returns a Monad type, as it sits in a do block

-- This code implements a Monad version of map, applying a function that returns a Monad to every element of a list
-- It does not use List as a Monad, and the function f can be of any monadic type