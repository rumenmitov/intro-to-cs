isBool :: a -> a -> Bool -> a
isBool f g b = case b of 
    True -> g
    False -> f

findSum :: Num a => [a] -> a
findSum list = case list of
    [] -> 0
    (x:xs) -> x + sum xs

binom :: Integral a => a -> a -> a
binom n k = case k == 0 of 
    True -> 1
    False -> case n == k of 
        True -> 1
        False -> binom (n-1) k + binom (n-1) (k-1)

fibx :: Integral a => a -> a
fibx num = case num of
    0 -> 0
    1 -> 1
    _ -> case num < 0 of
            True -> fibx (num+2) + (num+1)
            False -> fibx (num-2) + (num-1)
