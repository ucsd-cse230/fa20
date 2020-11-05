module Lec_11_5_20 where

-- >>> 2.1 + 3.2
-- 5.300000000000001

-- >>> True + True
-- No instance for (Num Bool) arising from a use of ‘+’


half :: Int -> Int
half x = x `div` 2


foo :: (Eq a) => a -> a -> Bool
foo x y = x == y

-- >>> :type (+)
-- (+) :: forall a. (Num a) => a -> a -> a

-- <A implements Num> A add(A x, A y)

-- >>> False / True
-- No instance for (Fractional Bool) arising from a use of ‘/’

instance Num Bool where
    (+) x y = x || y
    (*) x y = x && y
    negate x = not x
    fromInteger 0 = False
    fromInteger _ = True

-- True + False 

-- True.add(False)

-- >>> ("dab", 10) < ("cat", 2)
-- False

-- >>> "apple" < "app"
-- False


-- >>> show 2
-- "2"


-- >>> show (1,2,"cat")
-- "(1,2,\"cat\")"


-- >>> :t show
-- show :: forall a. Show a => a -> String

data Unshowable = A | B | C

thingA :: Unshowable
thingA = A

thingB :: Unshowable
thingB = B

things :: [Unshowable]
things = [thingA, thingB]

-- >>> thingA == thingA 

-- (A) True
-- (B) False 
-- (C) Run-time ERROR
-- (D) Compile Time ERROR
-- (E) 2








