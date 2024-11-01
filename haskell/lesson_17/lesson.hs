import Test.HUnit
import Data.Char
import Data.List
import Data.Function

-- return last element of list
myLast :: [a] -> a
myLast xs = head (reverse xs)

-- using function composition and function decomposition
myLast' :: [a] -> a
myLast' = (head . reverse)

-- contains all leters in alphabet
isPangram :: String -> Bool
isPangram s = letters s == ['a'..'z']
    where letters = nub . sort . map toLower . filter isLetter

-- using $
isPangram' :: String -> Bool
isPangram' s = letters == ['a'..'z']
    where letters = nub $ sort $ map toLower $ filter isLetter s

-- using &
isPangram_2 :: String -> Bool
isPangram_2 s = letters == ['a'..'z']
    where letters = s & filter isLetter & map toLower & sort & nub

pangramTests = TestList [
    isPangram quickfox ~?= True,
    isPangram "" ~?= False,
    isPangram "abcxyz" ~?= False,
    isPangram' quickfox ~?= True,
    isPangram' "" ~?= False,
    isPangram' "abcxyz" ~?= False,
    isPangram_2 quickfox ~?= True,
    isPangram_2 "" ~?= False,
    isPangram_2 "abcxyz" ~?= False
    ]
    where quickfox = "The quick brown fox jumps over the lazy dog."

main = runTestTT pangramTests
