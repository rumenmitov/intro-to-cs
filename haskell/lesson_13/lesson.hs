-- let in expression
isVowel :: Char -> Bool
isVowel ch = let vowels = "aeouiAeoui"
    in ch `elem` vowels

-- where expression
isVowel' :: Char -> Bool
isVowel' ch = ch `elem` vowels
    where vowels= "aeouiAeoui"

-- distance formula
distance :: Floating a => (a, a) -> (a, a) -> a
distance (px, py) (qx, qy) = sqrt (sq dx + sq dy)
    where dx = qx - px
          dy = qy - py
          sq x = x * x

-- distance formula using let in
distance' :: Floating a => (a, a) -> (a, a) -> a
distance' (px, py) (qx, qy) = let dx = qx - px
                                  dy = qy - py
                                  sq x = x * x
    in sqrt (sq dx + sq dy)

-- percentage calculator
showPct :: Int -> String
showPct p
    | p < 45 = show p ++ "% (failing)"
    | otherwise = show p ++ "% (passing)"

showPct' :: Int -> String
showPct' p
    | failing = show p ++ "% (failing)"
    | passing = show p ++ "% (passing)"
    where failing = p < 45
          passing = not failing


