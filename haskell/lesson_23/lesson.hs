class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

    (==) a b = not (a Main./= b) -- default implementation
    (/=) a b = not (a Main.== b) -- default implementation

isElem :: Main.Eq a => a -> [a] -> Bool
isElem _ [] = False
isElem y (x:xs) = y Main.== x || isElem y xs

data Size = XS | S | M | L | XL deriving (Prelude.Eq, Prelude.Ord)

-- instance Eq Size where
--     XS == XS = True
--     S == S = True
--     M == M = True
--     L == L = True
--     XL == XL = True
--     _ == _ = False

data Ordering = LT | EQ | GT deriving (Prelude.Eq)

class (Main.Eq a) => Ord a where
    (<), (>), (<=), (>=) :: a -> a -> Bool
    compare :: a -> a -> Main.Ordering
    max, min :: a -> a -> a

    compare x y | x Main.== y = Main.EQ   -- default implementation
                | x Main.< y = Main.LT
                | otherwise = Main.GT

    x < y = Main.compare x y Prelude.== Main.LT
    x <= y = Main.compare x y Prelude./= Main.GT
    x > y = Main.compare x y Prelude.== Main.GT
    x >= y = Main.compare x y Prelude./= Main.LT

    max x y | x Main.<= y = y
            | otherwise = x

    min x y | x Main.<= y = x
            | otherwise = y

