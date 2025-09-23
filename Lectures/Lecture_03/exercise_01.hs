-- Given the function definition, what is the type of this function?
twice f x = f (f (x))

-- > :t twice
-- > twice :: (a -> a) -> a -> a

twicetwo (f, x) = f (f (x))

-- > :t twicetwo
-- > twicetwo :: (a ->, a) -> a
