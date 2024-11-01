data List a = Nil | Cons a (List a) deriving (Show)

f :: [a] -> List a
f [] = Nil
f (x:xs) = Cons x (f xs)

(.:.) :: a -> List a -> List a
(.:.) x y = Cons x y
infixr 5 .:.

data Op = Add | Sub | Mul | Div deriving (Show)

data IntExpTree = Val Int | Exp IntExpTree Op IntExpTree deriving (Show)

m :: (Int -> Int) -> IntExpTree -> IntExpTree
m f (Val c) = Val (f c)
m f (Exp l o r) = Exp (m f l) o (m f r)

e :: IntExpTree -> Int
e (Val n) = n
e (Exp l o r) = let el = e l
                    er = e r
                in case o of
                     Add -> el + er
                     Sub -> el - er
                     Mul -> el * er
                     Div -> div el er

x = Exp (Val 4) Mul (Exp (Val 5) Add (Val 2))
