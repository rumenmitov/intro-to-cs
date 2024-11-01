import Test.HUnit

-- (B ∨ F) ∧ ((B ∧ F) ⊼ I) ∧ ((I ∨ ¬B) ⇒ ¬F)
-- NOTE: A ⇒ B is equivalent to ¬(A ∧ ¬B)
diet :: Bool -> Bool -> Bool -> Bool
diet b f i = (b || f) && (not ((b && f) && i)) && (not ((i || not b) && not (not f)))

-- D(B, F, I) = B ∧ ¬(B ∧ F ∧ I)
diet' :: Bool -> Bool -> Bool -> Bool
diet' b f i = b && not (b && f && i)

tests = TestList [
    diet False False False  ~?= False,
    diet False False True  ~?= False,
    diet False True False  ~?= False,
    diet False True True  ~?= False,
    diet True False False  ~?= True,
    diet True False True  ~?= True,
    diet True True False  ~?= True,
    diet True True True  ~?= False
    ]

tests' = TestList [
    diet' False False False  ~?= False,
    diet' False False True  ~?= False,
    diet' False True False  ~?= False,
    diet' False True True  ~?= False,
    diet' True False False  ~?= True,
    diet' True False True  ~?= True,
    diet' True True False  ~?= True,
    diet' True True True  ~?= False
    ]

truthTable :: (Bool -> Bool -> Bool -> Bool) -> [(Bool, Bool, Bool, Bool)]
truthTable f = [
    (False, False, False, f False False False),
    (False, False, True, f False False True),
    (False, True, False, f False True False),
    (False, True, True, f False True True),
    (True, False, False, f True False False),
    (True, False, True, f True False True),
    (True, True, False, f True True False),
    (True, True, True, f True True True)
    ]

dietTest = TestList [
    truthTable diet ~?= truthTable diet'
    ]
main = runTestTT $ TestList [tests, tests', dietTest]
