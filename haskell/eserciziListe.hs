sommaPrec :: (Eq a, Num a) => [a] -> Bool 
sommaPrec = aux 0
    where
        aux somma [x] = somma == x
        aux somma (x : xs) = aux (somma + x) xs

{-
ultimoSomma :: (Eq a, Num a) => [a] -> Bool
ultimoSomma xs = head ys == sum (tail ys)
    where
        ys = reverse xs
-}
sommaPrecStandard :: (Eq a, Num a) => [a] -> Bool 
sommaPrecStandard list = sum (take (length list - 1) list) == head (drop (length list - 1) list)

{-
piuLunga :: [[a]] -> [[a]]
piuLunga xs = filter ((== m) . length) xs
    where
        m = maximum (map length xs)
-}
listeMax :: Ord a => [[a]] -> [[a]]
listeMax list = [ x | x <- list, length x == length (maximum list) ]

mapRid :: (a -> b) -> [a] -> [b]
mapRid f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr aux []
    where
        aux x xs | p x = x : xs
                 | otherwise = xs

{-
sottoLista :: Eq a => [a] -> [a] -> Bool
sottoLista [] _ = True
sottoLista _ [] = False
sottoLista (x : xs) (y : ys) | x == y = sottoLista xs ys
sottoLista xs (_ : ys) = sottoLista xs ys
-}
eSottoLista :: (Eq a, Num a) => [a] -> [a] -> Bool 
eSottoLista [] l2 = True 
eSottoLista l1 [] = False
eSottoLista (x : l1) (y : l2)   | x == y = eSottoLista l1 l2
                                | otherwise = eSottoLista (x : l1) l2

sottoListe :: [a] -> [[a]]
sottoListe [] = [[]]
sottoListe (x : xs) = xss ++ map (x :) xss
    where
        xss = sottoListe xs