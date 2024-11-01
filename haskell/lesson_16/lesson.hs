import Test.HUnit

-- increment number
incr :: Int -> Int
incr = (+ 1)

-- return list of first n elements
myTake :: Int -> [a] -> [a]
myTake n xs
    | null xs = []
    | n <= 0  = []
    | otherwise = head xs : myTake (n-1) (tail xs)

-- using case expressions
myTake' :: Int -> [a] -> [a]
myTake' n xs = case null xs of
    True -> []
    False -> case n <= 0 of
        True -> []
        False -> head xs : myTake (n-1) (tail xs)

-- using lambda and currying
myTake_2 :: Int -> [a] -> [a]
myTake_2 = \n -> (\xs -> case null xs of
    True -> []
    False -> case n <= 0 of
        True -> []
        False -> head xs : myTake (n-1) (tail xs))

takeTests = TestList [
    myTake 5 "" ~?= "",
    myTake (-5) "" ~?= "",
    myTake 5 "abc" ~?= "abc",
    myTake 3 [1..] ~?= [1,2,3],
    myTake' 5 "" ~?= "",
    myTake' (-5) "" ~?= "",
    myTake' 5 "abc" ~?= "abc",
    myTake' 3 [1..] ~?= [1,2,3],
    myTake_2 5 "" ~?= "",
    myTake_2 (-5) "" ~?= "",
    myTake_2 5 "abc" ~?= "abc",
    myTake_2 3 [1..] ~?= [1,2,3]
    ]

main = runTestTT takeTests
