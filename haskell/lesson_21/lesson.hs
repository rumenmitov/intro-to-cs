import Test.HUnit
import Data.Char

data Gender = Female | Male
    deriving (Show, Eq)

data Person = Person {
    name :: String,
    gender :: Gender
}
    deriving (Show)

personTest = TestList [
    name joe ~?= "Joe Smith",
    gender joe ~?= Male,
    name lucy ~?= "Lucy Smith",
    gender lucy ~?= Female
    ]
    where joe = Person { name = "Joe Smith", gender = Male }
          lucy = Person { name = "Lucy Smith", gender = Female }

data Family a = Single a | Couple {
    woman :: a,
    man :: a,
    descendants :: [Family a]
}

    deriving (Show)

isSingle :: Family a -> Bool
isSingle (Single _) = True
isSingle (Couple _ _ _) = False 

isCouple :: Family a -> Bool
isCouple (Single _) = False
isCouple (Couple _ _ _) = True 

famMap :: (a -> b) -> Family a -> [b]
famMap g (Single p) = [g p]
famMap g (Couple w m xs) = [g w, g m] ++ concat (Prelude.map (famMap g) xs)

familyTest = TestList [
    isCouple family ~?= True,
    isSingle family ~?= False,
    all isSingle (descendants family) ~?= True
    ]
    where joe = Person { name = "Joe Smith", gender = Male }
          lucy = Person { name = "Lucy Smith", gender = Female }
          adam = Person { name = "Adam Smith", gender = Male }
          eve = Person { name = "Eve Smith", gender = Female }
          family = Couple { woman = eve, man = adam, 
              descendants = [Single joe, Single lucy] }

tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (x:xs) = Just xs

data ParseDigitError = NotADigit Char
    deriving (Show)

parseDigit :: Char -> Either ParseDigitError Int 
parseDigit c
    | isDigit c = Right (ord c - ord '0')
    | otherwise = Left (NotADigit c)
