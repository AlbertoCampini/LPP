module Generici.Tree where

data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving Show

empty :: Tree a -> Bool
empty Leaf = True
empty _ = False

depth :: Tree a -> Int
depth Leaf = 0
depth (Branch _ t1 t2) = 1 + max (depth t1) (depth t2)

leaves :: Tree a -> Int
leaves Leaf = 1
leaves (Branch _ t1 t2) = leaves t1 + leaves t2

elements :: Tree a -> [a]
elements Leaf = []
elements (Branch x t1 t2) = elements t1 ++ [x] ++ elements t2

tmax :: Tree a  -> a
tmax (Branch x _ Leaf) = x
tmax (Branch _ _ t) = tmax t

ordList :: Ord a => [a] -> Bool
ordList [] = True
ordList [_] = True
ordList (x : y : xs) = x <= y && ordList(y:xs)

binarySearch :: Ord a => Tree a -> Bool
binarySearch x = ordList (elements x)


data NTree a = Node a [NTree a] deriving Show

count :: NTree a -> Int
count (Node a ts) = 1 + aux ts
  where
    aux [] = 0
    aux (x : xs) = count x + aux xs

maxt :: Ord a => NTree a -> a
maxt ts = maximum (aux ts)
 where
  aux (Node x ts) = [x] ++ concat (map aux ts)

