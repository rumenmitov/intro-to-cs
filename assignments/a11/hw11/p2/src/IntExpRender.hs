module IntExpRender where

import IntExp

render :: Exp -> String
render (C x) = show x -- return constant
render (V name) = name -- return variable name
render (S e1 e2) = "(" ++ render e1 ++ " + " ++ render e2 ++ ")" -- evaluate each expression, wrap it in parenthesis and add + sybol in infix notation
render (P e1 e2) = "(" ++ render e1 ++ " * " ++ render e2 ++ ")" -- evaluate each expression, wrap it in parenthesis and add * sybol in infix notation
