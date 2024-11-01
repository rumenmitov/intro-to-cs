import Test.HUnit

data BTree a = Nil
    | Leaf a
    | Node (BTree a) a (BTree a) 
    deriving (Eq, Show)


treeToList :: BTree a -> [a]
treeToList Nil = []
treeToList (Leaf x) = [x]
treeToList(Node l x r) = (treeToList l) ++ [x] ++ (treeToList r)

listToTree :: [a] -> BTree a
listToTree [] = Nil
listToTree [x] = Leaf x
listToTree xs = Node  (listToTree prefix) x (listToTree suffix)
    where (prefix, x:suffix) = splitAt (length xs `div` 2) xs


toListTest = TestList [
    treeToList (listToTree "") ~?= "",
    treeToList (listToTree "h") ~?= "h",
    treeToList (listToTree "hello") ~?= "hello"
    ]

toTreeTest = TestList [
    listToTree [] ~?= (Nil :: BTree Int),
    listToTree [1] ~?= Leaf 1,
    listToTree [1..2] ~?= Node (Leaf 1) 2 Nil,
    listToTree [1..3] ~?= Node (Leaf 1) 2 (Leaf 3)
    ]

checkElem :: Eq a => a -> BTree a -> Bool
checkElem _ Nil = False
checkElem e (Leaf x) = e == x
checkElem e (Node l x r) = e == x || checkElem e l || checkElem e r

elemTests = TestList [
    checkElem 'a' Nil ~?= False,
    checkElem 'a' (Leaf 'a') ~?= True,
    checkElem 'b' (Leaf 'a') ~?= False,
    checkElem 'h' (listToTree "hello") ~?= True,
    checkElem 'o' (listToTree "hello") ~?= True,
    checkElem 'a' (listToTree "hello") ~?= False
    ]

elemOrdered :: Ord a => a -> BTree a -> Bool
elemOrdered _ Nil = False
elemOrdered e (Leaf x) = e == x
elemOrdered e (Node l x r)
    | e == x = True
    | e < x = elemOrdered e l
    | otherwise = elemOrdered e r

elemOrdTests = TestList [
    elemOrdered 'a' Nil ~?= False,
    elemOrdered 'a' (Leaf 'a') ~?= True,
    elemOrdered 'b' (Leaf 'a') ~?= False,
    elemOrdered 'h' (listToTree "hello") ~?= True,
    elemOrdered 'o' (listToTree "hello") ~?= True,
    elemOrdered 'a' (listToTree "hello") ~?= False
    ]
