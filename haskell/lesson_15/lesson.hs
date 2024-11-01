import Data.Char

getLetterOrDigits :: String -> String
getLetterOrDigits xs = filter (\c -> isLetter c || isDigit c) xs

add :: Num a => a -> a -> a
add = \x y -> (+) x y

add' :: Num a => a -> a -> a
add' = \x -> \y -> (+) x y



