data Err a = Result a | Wrong Float deriving Show

-- safelog :: Float -> Err Float
safelog n | n > 0 = Result (log n)
          | otherwise = Wrong n

instance Functor Err where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Result n) = Result (f n)
    fmap _ (Wrong n) = Wrong n

instance Applicative Err where
    -- pure :: a -> Err a
    pure = Result

    -- (<*>) :: Err (a -> b) -> Err a -> Err b
    (Result n) <*> x = fmap n x
    (Wrong n) <*> (Wrong x) = Wrong x

