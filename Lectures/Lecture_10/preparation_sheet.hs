{-
Exercise 01:
An onion consists of a finite number of layers surrounding a core.
In this problem, we let the core be a value. Figure 0.1 shows an onion
with six layers and core "bingo". Below is a declaration of an algebraic 
Datatype Onion a parametrized by the type a.

data Onion a = Core a | Layer (Onion a)

Define Onion as an instance of Functor.
Hint: be inspired by how the book shows how one can let the Tree type
become an instance of Functor.
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

onion = Layer (Layer (Core "bingo"))

-- fmap (\x -> 10) onion
-- > Layer (Layer (Core 10))

-- pure (\x -> "NEW WORLD") <*> onion
-- > Layer (Layer (Core "NEW WORLD"))

{-
Exercise 02:
Check that the first two applicative laws at the top of page 163 hold
true for the Maybe type. Hint: Use the definitions of pure and <*> on page 160.
-}
{-
Applicative Laws
In addition to providing the functions pure and <*>, applicative functors are
also required to satisfy four equational laws:

1. pure id <*> x   = x
2. pure (g x)      = pure g <*> pure x
3. x <*> pure y    = pure (\g -> g y) <*> x
4. x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-}

-- Law 1
-- pure id <*> Just 1
-- > Just 1
-- (pure (id (Just 1)))
-- > Just 1

-- Law 2
-- pure ((+1) 0)
-- > 1
-- pure (+1) <*> pure 0
-- > 1
