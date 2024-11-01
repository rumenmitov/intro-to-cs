import Test.HUnit

-- NOTE Word == unsigned integer
type Name = String
type Number = Word
type Year = Word
type Month = Word
type Day = Word

-- data Student = Student Name Number Year Age
-- joe = Student "Joe Smith" 12345 2022 20


-- name :: Student -> String
-- name (Student name _ _ _) = name

-- number :: Student -> Int
-- number (Student _ num _ _) = num

-- enrollment :: Student -> Int
-- enrollment (Student _ _ year _) = year

-- age :: Student -> Int
-- age (Student _ _ _ age) = age

data Date = Date {
    year :: Year,
    month :: Month,
    day :: Day
    }

    deriving (Show, Eq)

data Student = Student {
    name :: Name,
         number :: Number,
         enrollment :: Year,
         birthday :: Date
}

    deriving (Show)

studentsTest = TestList [
    name joe ~?= "Joe Smith",
    number joe ~?= 12345,
    enrollment joe ~?= 2022,
    birthday joe ~?= (Date 2002 4 1)
    ]
    where joe = Student {
        name = "Joe Smith",
        number = 12345,
        enrollment = 2022,
        birthday = Date { year = 2002, month = 4, day = 1 }
        }
