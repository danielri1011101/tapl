data Tree a = EmptyTree | Node a (Tree a) (Tree a)

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node y leftT rightT)
  | x < y = Node y (treeInsert x leftT) rightT
  | x > y = Node y leftT (treeInsert x rightT)
  | otherwise = Node x leftT rightT
