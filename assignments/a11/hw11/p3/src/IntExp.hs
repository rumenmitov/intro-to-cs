module IntExp where

data Exp = C Int -- a constant integer
    | V String -- a variable with a name
    | S Exp Exp -- a sum of two expressions
    | P Exp Exp -- a product of two expressions
    deriving (Show, Eq)
