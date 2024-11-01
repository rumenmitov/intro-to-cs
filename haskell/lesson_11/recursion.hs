import Test.HUnit

getGCD :: Integral a => a -> a -> a
getGCD a 0 = a
getGCD a b = getGCD b (a `mod` b)

getSum :: Num a => [a] -> a
getSum [] = 0
getSum (head : tail) = head + getSum tail

getProduct :: Num a => [a] -> a
getProduct [] = 0
getProduct [x] = x
getProduct (lHead : lTail) = lHead * (getProduct lTail)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList myList = last myList : reverseList (init myList)



testSuit = TestList [ getSum [] ~?= 0,
                      getSum [42] ~?= 42,
                      getSum [1..10] ~?= 55,
                      getSum [-10..0] ~?= -55,
                      getSum [-10..10] ~?= 0,

                      getProduct [] ~?= 0,
                      getProduct [42] ~?= 42,
                      getProduct [2, 4, 8, 16] ~?= 1024,
                      getProduct [1..10] ~?= 3628800,
                      getProduct [-10..(-1)] ~?= 3628800,
                      getProduct [-10..10] ~?= 0, 

                      reverseList "" ~?= "",
                      reverseList "X" ~?= "X",
                      reverseList (reverseList "original") ~?= "original",
                      reverseList [1..5] ~?= [5,4..1]
                    ]

main = runTestTT testSuit
