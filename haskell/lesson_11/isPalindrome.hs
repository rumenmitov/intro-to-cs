import Test.HUnit

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome str = (head str == last str) && isPalindrome (take ((length str) - 2) (tail str))

tests = TestList [
    isPalindrome [] ~?= True,
    isPalindrome "level" ~?= True,
    isPalindrome "tree" ~?= False
    ]

main = runTestTT tests
