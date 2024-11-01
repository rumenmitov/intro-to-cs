data BTree a = Nil | Leaf a | Node (BTree a) a (BTree a) deriving (Eq, Show)


productTree :: Num t => BTree t -> t
productTree Nil = 0
productTree (Leaf v) = v
productTree (Node l v r) = v * (productTree l) * (productTree r)

tree = Node (Leaf 5) 4 (Leaf 2)
