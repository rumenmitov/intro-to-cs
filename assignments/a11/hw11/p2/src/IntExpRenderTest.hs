module IntExpRenderTest (main) where

import Test.HUnit
import IntExp
import IntExpRender

tests = TestList [ 
      render (C 42) ~?= "42"
    , render (V "foo") ~?= "foo"
    , render (S (C 0) (C 1)) ~?= "(0 + 1)"
    , render (P (C 0) (C 1)) ~?= "(0 * 1)"
    , render (P (S (C 0) (C 1)) (S (C 42) (V "foo"))) ~?= "((0 + 1) * (42 + foo))"
    ]

main :: IO Counts
main = runTestTT tests
