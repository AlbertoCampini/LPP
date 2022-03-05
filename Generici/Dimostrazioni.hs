module Generici.Dimostrazioni where

length :: Ord a => [a] -> Int
length [] = 0
length (x : xs) = 1 + length xs

tail :: Ord a => [a] -> [a]
tail (_ : xs) = xs 

(++) :: Ord a => [a] -> [a] -> [a]
(++) [] ys = xs
(++) (x : xs) ys = x : (++) xs ys

reverse :: Ord a => [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

even :: Ord a => [a] -> [a]
even [] = []
even (x : xs) = x : odd xs 

odd :: Ord a => [a] -> [a]
odd [] = []
odd (x : xs) = even xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x : y :xs) = x <= y && sorted (y : xs)

map :: Ord a b => (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = fx : map f xs

mapFoldr :: Ord a b => (a -> b) -> [a] -> [b]
mapFoldr f = foldr ((:) . f) []

concat :: Ord a => [[a]] -> [a]
concat [] = []
concat ([] : xss) = concat xss
concat ((x : xs) : xss) [x] ++ concat (xs : xss)

sum :: Num a => [a] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

filter :: Ord a b => (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) | f x = x : filter f xs
                  | otherwise = filter f xs
                  
all :: Ord a => (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x : xs) = f x && all f xs

zip :: Ord a b => [a] -> [b] -> [(a,b)]
zip :: [] _ = []
zip :: _ [] = []
zip (x : xs) (y : ys) = (x,y) : zip xs ys

uncurry :: Ord a b c => (a -> b -> c) -> (a,b) -> c 
uncurry f (x, y) = f x y

foldr :: Ord a => (a -> b -> c) -> [a] -> b 
foldr _ x [] = x
foldr f x (y : ys) = f y (foldr f x ys)

foldl :: Ord a => (a -> b -> c) -> [a] -> b 
foldl _ x [] = x
foldl f x (y : ys) = foldl f (f x y) ys 