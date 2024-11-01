import Test.HUnit

binom :: Integral a => a -> a -> a
binom n k
  | k == 0  = 1
  | n == k  = 1
  | otherwise = binom (n-1) k + binom (n-1) (k-1)

binomTests = TestList [
            [ binom 0 x | x <- [0..0] ] ~?= [1],
            [ binom 1 x | x <- [0..1] ] ~?= [1, 1],
            [ binom 2 x | x <- [0..2] ] ~?= [1, 2, 1],
            [ binom 3 x | x <- [0..3] ] ~?= [1, 3, 3, 1],
            [ binom 4 x | x <- [0..4] ] ~?= [1, 4, 6, 4, 1]
    ]

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (lHead:lTail) = lHead + mySum lTail

mySum' :: Num a => [a] -> a
mySum' list
  | null list = 0
  | otherwise = head list + sum (tail list)

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibTests = TestList [ map fib [0..9] ~?= [0,1,1,2,3,5,8,13,21,34] ]
{-
    passing a negative number doesn't work, produces error
    this might arise because fib (n-1) + fib (n-2) will go on forever

    Answer:

    fib -2 -> error, should be fib (-2)
    fib (-2) -> stack overflow, endless loop
-}

fib' :: Integral a => a -> a
fib' n
  | n == 0 = 0
  | n == 1 = 1
  | n == -1 = -1
  | n < -1 = fib' (n+2) + fib' (n+1)
  | otherwise = fib' (n-1) + fib' (n-2)

fib'Tests = TestList [ map fib' [-6..6] ~?= [-8,-5,-3,-2,-1,-1,0,1,1,3,5,8] ]

fib'' :: Integral a => a -> a
fib'' 0 = 0
fib'' 1 = 1
fib'' (-1) = -1
fib'' n
    | n < -1 = fib (n+2) + fib (n+1)
    | otherwise = fib (n+1) + fib (n+2)

fib''Tests = TestList [ map fib'' [-6..6] ~?= [-8,-5,-3,-2,-1,-1,0,1,1,3,5,8] ]

main = runTestTT binomTests
