module Generici.NTree where

data Tree a = Empty | Node a [Tree a] deriving Show

elements :: Tree a -> [a]
elements Empty = []
elements (Node x ts) = [x] ++ (concat (aux elements ts))
  where
    aux f [] = []
    aux f (x : ts) = f x : aux f ts


maxElem :: Tree Int -> Int
maxElem Empty = 0
maxElem (Node x ts) = maximum (aux (Node x ts))
  where
    aux Empty = []
    aux (Node x ts) = [x] ++ (concat (map aux ts))


treelen :: Tree a -> Int
treelen Empty       = 0
treelen (Node x ts) = 1 + sum (map (treelen) ts)

normalize :: Tree a -> Tree a
normalize Empty = Empty
normalize (Node x t) = Node x (concat (map (aux.normalize) t))
  where aux Empty = []
        aux t = [t]

count :: Tree a -> Int
count (Node _ xs) = 1 + aux xs
  where aux [] = 0
        aux (x:xs) = count x + aux xs

