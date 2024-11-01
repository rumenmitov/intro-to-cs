-- Return the list of positive divisors of an integer n.
divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n], mod n x == 0 ]

-- Return the sum of divisors of n taken to the power of z
sigma :: Int -> Int -> Int
sigma z n = sum [x ^ z | x <- divisors n]
