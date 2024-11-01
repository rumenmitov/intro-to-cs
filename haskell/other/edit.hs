import Test.HUnit

editStr :: Ord a => [a] -> [a] -> Int
editStr [] ys = length ys
editStr xs [] = length xs
editStr (x:xs) (y:ys)
    | x == y = editStr xs ys
    | otherwise = 1 + findMin [ editStr (x:xs) ys, editStr xs (y:ys), editStr xs ys ]

findMin :: [Int] -> Int
findMin [x] = x
findMin (x:xs) = min x (findMin xs)

tests = TestList [
    editStr "" "hello" ~?= 5,
    editStr "hello" "" ~?= 5,
    editStr "foo" "boo" ~?= 1,
    editStr "foo" "fbo" ~?= 1,
    editStr "dog" "water" ~?= 5,
    editStr [1..5] [5,4..1] ~?= 4
    ]


main = runTestTT tests
