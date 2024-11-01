sum :: Num a => [a] -> a
sum = foldr (+) 0

sum' :: Num a => [a] -> a
sum' = foldl (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

product' :: Num a => [a] -> a
product' = foldl (*) 1

length :: Num a => [a] -> a
length = foldr (\x _ -> x + 1) 0

length' :: Num a => [a] -> a
length' = foldl (\x _ -> x + 1) 0

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myMap' :: (a -> b) -> [a] -> [b]
myMap' f = foldl (\ys y -> ys ++ [f y]) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldl (\xs x -> if f x then xs ++ [x] else xs) []

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' f = foldr (\x xs -> if f x then x : xs else xs) []
