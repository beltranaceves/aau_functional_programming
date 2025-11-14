import qualified Prelude
import Prelude hiding (Functor, fmap)


type ArrowInt a = (Int -> a)

type ArrowR r a = (r -> a)

instance Functor ArrowInt where
    
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (r -> a) -> f r -> f a 
    

{-

-}