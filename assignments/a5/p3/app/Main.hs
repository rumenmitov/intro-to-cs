module Main where
import Data.Word
import Test.HUnit

-- Convert a non-negative integer number into a String providing a
-- binary representation of the number.
dtob :: Word -> String
dtob 0 = "0"
dtob 1 = "1"
dtob num = dtob (div num 2) ++ show (mod num 2)

-- | Convert a String representing a non-negative integer number as a
-- binary number into a non-negative integer number.
btod :: String -> Word
btod "" = 0
btod "0" = 0
btod num = (+) ((*) (read [head num] :: Word) $ (^) 2 $ length num - 1) $ btod $ tail num

-- Below are some test cases...

dtobTests = TestList [ dtob 0 ~?= "0"
                     , dtob 1 ~?= "1"
                     , dtob 2 ~?= "10"
                     , dtob 127 ~?= "1111111"
                     , dtob 12345 ~?= "11000000111001"
                     ]

btodTests = TestList [ btod "0" ~?= 0
                     , btod "1" ~?= 1
                     , btod "10" ~?= 2
                     , btod "1111111" ~?= 127
                     , btod "11000000111001" ~?= 12345
                     ]

main = runTestTT $ TestList [ dtobTests, btodTests ]

