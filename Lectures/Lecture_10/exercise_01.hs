data UTree a = Node a [UTree a] deriving Show

instance Functor UTree where

    -- fmap :: (a -> b) -> UTree a -> UTree b
    fmap f (Node val branches) = Node (f val) (fmap (fmap f) branches)
    -- fmap f (Node val branches) = Node (f val) ([fmap f n | n <- branches])
    -- fmap f (Node val branches) = Node (f val) (foldBranches (fmap f) branches)

-- foldBranches f xs = foldr p v xs 
--                     where
--                         v = [] 
--                         p = (:) . f
utree = Node 1 [Node 2 [Node 3 []], Node 4 [Node 5 []], Node 6 [Node 7[]]]

-- fmap (+1) utree
-- > Node 2 [Node 3 [Node 4 []],Node 5 [Node 6 []],Node 7 [Node 8 []]]