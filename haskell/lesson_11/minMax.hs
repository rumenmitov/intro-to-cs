import Test.HUnit

isValid :: Integral a => Bool -> a -> a -> a
isValid True x _ = x
isValid False _ y = y

findMinMax :: Integral a => [Char] -> [a] -> a
findMinMax _ [] = 0
findMinMax "min" (lHead : lTail) = isValid (null lTail) lHead (min lHead (findMinMax "min" lTail))
findMinMax "max" (lHead : lTail) = isValid (null lTail) lHead (max lHead (findMinMax "max" lTail))

tests = TestList [
    findMinMax "max" [] ~?= 0,
    findMinMax "min" [] ~?= 0,
    findMinMax "max" [2] ~?= 2,
    findMinMax "min" [2] ~?= 2,
    findMinMax "max" [1..10] ~?= 10,
    findMinMax "min" [1..10] ~?= 1,
    findMinMax "max" [-10..0] ~?= 0,
    findMinMax "min" [-10..0] ~?= -10,
    findMinMax "max" [-10..10] ~?= 10,
    findMinMax "min" [-10..10] ~?= -10
    ]

main = runTestTT tests
