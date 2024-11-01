import Test.HUnit

largest :: Ord a => [a] -> a
largest [] = error "there is no largest in an empty list"
largest [x] = x
largest (lHead: lTail) = if lHead > largest lTail then lHead else largest lTail

largestTest = TestList [
    largest [0] ~?= 0,
    largest "hello" ~?= 'o',
    largest [1,2,3] ~?= 3,
    largest [3,2,1] ~?= 3,
    largest [3,3,3] ~?= 3
    ]

main = runTestTT largestTest
