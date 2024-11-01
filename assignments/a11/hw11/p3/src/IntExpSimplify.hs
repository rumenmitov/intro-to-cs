module IntExpSimplify where

import IntExp


simplify :: Exp -> Exp 

-- matching the case expressions

simplify (S (C a) (C b)) =  C (a + b) -- a + b

simplify (S (V y) (C 0)) = V y -- y + 0 = y
simplify (S (C 0) (V y)) = V y -- 0 + y = y

simplify (S (C a) (S (C b) (V y))) = S (C (a + b)) (V y) -- a + (b + y) = (a + b) + y
simplify (S (C a) (S (V y) (C b))) = S (C (a + b)) (V y) -- a + (y + b) = (a + b) + y
simplify (S (S (C b) (V y)) (C a)) = S (C (a + b)) (V y) -- (b + y) + a = (a + b) + y
simplify (S (S (V y) (C b)) (C a)) = S (C (a + b)) (V y) -- (y + b) + a = (a + b) + y

simplify (P (C a) (C b)) = C (a * b) -- a * b

simplify (P (V x) (C 1)) = V x -- x * 1 = x
simplify (P (C 1) (V x)) = V x -- 1 * x = x

simplify (P (V x) (C 0)) = C 0 -- x * 0 = 0
simplify (P (C 0) (V x)) = C 0 -- 0 * x = 0

simplify (P (C a) (P (C b) (V y))) = P (C (a * b)) (V y) -- a * (b * y) = (a * b) * y
simplify (P (C a) (P (V y) (C b))) = P (C (a * b)) (V y) -- a * (y * b) = (a * b) * y
simplify (P (P (C b) (V y)) (C a)) = P (C (a * b)) (V y) -- (b * y) * a = (a * b) * y
simplify (P (P (V y) (C b)) (C a)) = P (C (a * b)) (V y) -- (y * b) * a = (a * b) * y

-- NOTE: the following are base cases for the recursive cases at the end
simplify (C a) = C a -- a = a
simplify (V x) = V x -- y = y

simplify (S (C a) (V x)) = S (C a) (V x) -- a + x = a + x
simplify (S (V x) (C a)) = S (V x) (C a) -- x + a = x + a

simplify (P (C a) (V x)) = P (C a) (V x) -- a * x = a * x
simplify (P (V x) (C a)) = P (V x) (C a) -- x * a = x * a

simplify (S (V x) (V y)) = S (V x) (V y) -- x + y = x + y

simplify (P (V x) (V y)) = P (V x) (V y) -- x * y = x * y

simplify (S e1 e2) = simplify (S (simplify e1) (simplify e2)) -- recursive sum
simplify (P e1 e2) = simplify (P (simplify e1) (simplify e2)) -- recursive product
