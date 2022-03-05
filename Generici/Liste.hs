module Generici.Liste where

union :: Ord a => [a] -> [a] -> [a]
union [] ys = ys
union xs [] = xs
union (x : xs) (y : ys) | x < y = [x] ++ union xs (y:ys)
                        | x > y = [y] ++ union (x : xs) ys
                        | x == y = [x] ++ union xs ys
                        
intersection :: Ord a => [a] -> [a] -> [a]
intersection [] _  = []
intersection _  [] = []
intersection (x : xs) (y : ys) | x == y    = x : intersection xs ys
                               | x < y     = intersection xs (y : ys)
                               | otherwise = intersection (x : xs) ys
                               
difference :: Ord a => [a] -> [a] -> [a]                               
difference [] _  =[] 
difference xs  [] = xs
difference (x : xs) (y : ys) | x == y    = difference xs ys
                               | x < y     = x : difference xs (y : ys)
                               | otherwise = difference (x : xs) ys
                               
concatMy :: Ord a => [[a]] -> [a]
concatMy [] = []
concatMy ([]:xss) = concatMy xss  
concatMy ((x : xs) : xss) = [x] ++ concatMy (xs : xss)



union1 :: Ord a => [a] -> [a] -> [a]
union1 xs ys = [x | x <- xs ] ++ [y | y <- ys, not (y `elem` xs)]