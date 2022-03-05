data Tree a = Empty | Node a [Tree a]

elements :: Tree a -> [a]
elements Empty = []
elements (Node a tree) = a : concatMap elements tree

normalize :: Tree a -> Tree a
normalize Empty = Empty
normalize (Node a tree) = Node a (concatMap (aux . normalize) tree)
    where
        aux Empty = []
        aux tree = [tree]