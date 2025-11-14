{-
For the aplicative functor for lists we have a definition
of the "funny star" composition <*> page 160. Give an alternative
recursive definition to it that uses fmap.
-}

{-
instance Applicative [] where
    pure x = [x]

    gs <*> xs = [g x | g <- gs, x <- xs]
-}

instance Applicative [] where

    pure x = [x]

    (g:gs) <*> (x:xs) = (g x):(gs <*> xs)