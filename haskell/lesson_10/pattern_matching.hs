showBool :: Bool -> String
showBool True = "this is true"
showBool False = "this is false"

boolFunc :: a -> a -> Bool -> a
boolFunc f _ False = f
boolFunc _ g True = g

showBool' :: Bool -> String
showBool' b = boolFunc "False" "True" b

zodiac :: Integral a => a -> String
zodiac 0 = "monkey"
zodiac 1 = "rooster"
zodiac 2 = "dog"
zodiac 3 = "pig"
zodiac 4 = "rat"
zodiac 5 = "ox"
zodiac 6 = "tiger"
zodiac 7 = "rabbit"
zodiac 8 = "dragon"
zodiac 9 = "snake"
zodiac 10 = "horse"
zodiac 11 = "sheep"
zodiac year = zodiac (year `rem` 12)

tuple1 :: (a, b, c) -> a
tuple1 (fst, _, _) = fst

tuple2 :: (a, b, c) -> b
tuple2 (_, snd, _) = snd

tuple3 :: (a, b, c) -> c
tuple3 (_, _, thrd) = thrd

dupHead :: [a] -> [a]
dupHead [] = []
dupHead myList = head myList : myList

dupHead2 :: [a] -> [a]
dupHead2 [] = []
dupHead2 (h : myList) = h : h : myList

dupSndEl :: [a] -> [a]
dupSndEl [] = []
dupSndEl [h] = [h]
dupSndEl (h : snd : listTail) = h : snd : snd :listTail
