data Tree a = Leaf | Branch a (Tree a) (Tree a)
    deriving Show

empty :: Tree a -> Bool
empty Leaf = True
empty _    = False

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Branch x Leaf Leaf
insert x t@(Branch y t1 t2) | x == y    = t
                            | x < y     = Branch y (insert x t1) t2
                            | otherwise = Branch y t1 (insert x t2)

elements :: Tree a -> [a]
elements Leaf             = []
elements (Branch x t1 t2) = elements t1 ++ [x] ++ elements t2

tmax :: Tree a -> a
tmax (Branch x _ Leaf) = x
tmax (Branch _ _ t) = tmax t

tmin :: Tree a -> a
tmin (Branch x Leaf _) = x
tmin (Branch _ t _) = tmin t

tminTot :: Tree a -> Maybe a
tminTot Leaf = Nothing 
tminTot (Branch x t _) = case y of
                        Nothing -> Just x
                        Just y  -> Just y
    where
        y = tminTot t

{-
treeSort :: Ord a => [a] -> [a]
treeSort = elements . foldr insert Leaf
-}
treeSort :: Ord a => [a] -> [a]
treeSort [] = []
treeSort list = elements (foldr insert Leaf list)

bst :: Ord a => Tree a -> Bool
bst Leaf = True 
bst (Branch x t1 t2) = bst t1 && bst t2 &&
                       (empty t1 || tmax t1 < x) &&
                       (empty t2 || x < tmin t2)