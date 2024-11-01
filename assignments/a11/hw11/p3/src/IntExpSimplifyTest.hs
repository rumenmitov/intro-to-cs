module IntExpSimplifyTest (main) where

import Test.HUnit
import IntExp
import IntExpSimplify

tI0 = TestList [ 
      simplify (C 3) ~?= C 3 -- 3 = 3
    , simplify (V "y") ~?= V "y" -- y = y
    ]

tS1 = TestList [ 
    simplify (S (C 3) (C 5)) ~?= C 8 -- 3 + 5 = 8
    ]

tS2 = TestList [ 
      simplify (S (C 0) (V "y")) ~?= V "y" -- 0 + y = y
    , simplify (S (V "y") (C 0)) ~?= V "y" -- y + 0 = y
    ]

tS3 = TestList [ 
      simplify (S (S (C 3) (V "y")) (C 5)) ~?= S (C 8) (V "y") -- (3 + y) + 5) = 8 + y
    , simplify (S (S (V "y") (C 3) ) (C 5)) ~?= S (C 8) (V "y") -- (y + 3) + 5) = 8 + y
    , simplify (S (C 3) (S (C 5) (V "y"))) ~?= S (C 8) (V "y") -- 3 + (5 + y) = 8 + y
    , simplify (S (C 3) (S (V "y") (C 5))) ~?= S (C 8) (V "y") -- 3 + (y + 5) = 8 + y
    ]

tS4 = TestList [ 
      simplify (S (S (C 3) (C 5)) (C 8)) ~?= C 16 -- (3 + 5) + 8 = 16
    , simplify (S (C 3) (S (C 5) (C 8))) ~?= C 16 -- 3 + (5 + 8) = 16
    , simplify (S (C 5) (V "y")) ~?= S (C 5) (V "y") -- 5 + y = 5 + y
    , simplify (S (V "y") (C 5)) ~?= S (V "y") (C 5) -- y + 5 = y + 5
    , simplify (S (V "x") (V "y")) ~?= S (V "x") (V "y") -- x + y = x + y
    ]

tP1 = TestList [ 
    simplify (P (C 3) (C 5)) ~?= C 15 -- 3 * 5 = 15
    ]

tP2 = TestList [ 
      simplify (P (C 1) (V "y")) ~?= V "y" -- 1 * y = y
    , simplify (P (V "y") (C 1)) ~?= V "y" -- y * 1 = y
    ]

tP3 = TestList [ 
      simplify (P (C 0) (V "y")) ~?= C 0 -- 0 * y = 0
    , simplify (P (V "y") (C 0)) ~?= C 0 -- y * 0 = 0
    ]

tP4 = TestList [ 
      simplify (P (P (C 3) (V "y")) (C 2)) ~?= P (C 6) (V "y") -- (3 * y) * 2) = 6 * y
    , simplify (P (P (V "y") (C 3) ) (C 2)) ~?= P (C 6) (V "y") -- (y * 3) * 2) = 6 * y
    , simplify (P (C 3) (P (C 2) (V "y"))) ~?= P (C 6) (V "y") -- 3 * (2 * y) = 6 * y
    , simplify (P (C 3) (P (V "y") (C 2))) ~?= P (C 6) (V "y") -- 3 * (y * 2) = 6 * y
    ]

tP5 = TestList [ 
      simplify (P (P (C 3) (C 5)) (C 8)) ~?= C 120 -- (3 * 5) * 8 = 120
    , simplify (P (C 3) (P (C 5) (C 8))) ~?= C 120 -- 3 * (5 * 8) = 120
    , simplify (P (C 5) (V "y")) ~?= P (C 5) (V "y") -- 5 * y = 5 * y
    , simplify (P (V "y") (C 5)) ~?= P (V "y") (C 5) -- y * 5 = y * 5
    , simplify (P (V "x") (V "y")) ~?= P (V "x") (V "y") -- x * y = x * y
    ]

tM0 = TestList [
    -- (2 * y) * (3 + (2 * 2)) = 14 * y
      simplify (P (P (C 2) (V "y")) (S (C 3) (P (C 2) (C 2)))) ~?= P (C 14) (V "y")
    -- x + (1 + -1) = x
    , simplify (S (V "x") (S (C 1) (C (-1)))) ~?= V "x"
    -- (1 + -1) * x = 0
    , simplify (P (S (C 1) (C (-1))) (V "x")) ~?= C 0
    -- (2 + -1) * x = x
    , simplify (P (S (C 2) (C (-1))) (V "x")) ~?= V "x"
    -- (2 * 2) * (3 + 4) = 28
    , simplify (P (P (C 2) (C 2)) (S (C 3) (C 4))) ~?= C 28
    ]

main = runTestTT $ TestList [tI0, tS1, tS2, tS3, tS4, tP1, tP2, tP3, tP4, tP5, tM0 ]

