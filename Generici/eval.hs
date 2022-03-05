import Data.Function

--EDIT HERE! ADD NEEDED FUNCTIONS!
--sample functions
cons :: a -> [a] -> [a]
cons x xs = x : xs

--example of modified function to avoid Prelude conflict
succ' :: Int -> Int
succ' x = x + 1

head' :: [a] -> a
head' (x : xs) = x

sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

bool' :: Int -> Bool
bool' x = True

negg' :: [a] -> Int -> a
negg' [a] _ = a

filter' :: (Ord a) => (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) | f x = x : filter' f xs
                  | otherwise = filter' f xs
--

aux :: Int -> Bool
aux a = True

expr = (\x -> \y -> x y)(\x -> (:) x) 1

