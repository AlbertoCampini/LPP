module Generici.Funzioni where

import Data.Char (ord,chr)

assoluto :: Int -> Int
assoluto n | n >= 0 = n
           | otherwise = negate n

successore :: Int -> Int
successore n = n + 1


pari :: Int -> Bool
pari n = n `mod` 2 == 0

bisestile :: Int -> Bool
bisestile n = n `mod` 4 == 0 && n `mod` 100 /= 0

dispari :: Int -> Bool
dispari n = n `mod` 2 /= 0

successorePari :: Int -> Int
successorePari n | pari n = successore n
                 | otherwise = assoluto n
somma :: [Int] -> Int
somma [] = 0
somma (x : xs) = x + somma xs

giorni :: Int -> Int
giorni n | bisestile n = 366
         | otherwise = 365

fattoriale :: Int -> Int
fattoriale n | n == 0 = 1
             | otherwise = n * fattoriale (n - 1)

sommaNumeri :: Int -> Int
sommaNumeri n | n == 0 = 0
              | otherwise = n + fattoriale (n - 1)

pow2 :: Int -> Int
pow2 n | n == 0 = 1
       | otherwise = 2 * pow2(n - 1)

bits :: Int -> Int
bits n | n == 0    = 0
       | pari n    = bits(n `div` 2)
       | otherwise = 1 + bits(n `div` 2)

potenzaDi2 :: Int -> Bool
potenzaDi2 n | n == 1  = True
             | n == 0 = False
             | otherwise = pari n && (potenzaDi2 (n `div` 2))

myconst :: a -> b -> a
myconst x y = x


media :: [Int] -> Float
media xs = fromIntegral (sum xs) / fromIntegral (length xs)


inversioni2 :: Ord a => [a] -> Int
inversioni2 [] = 0
inversioni2 [_] = 0
inversioni2 (x : y : xs) | x > y = 1 + inversioni(y : xs)
                         | otherwise = inversioni(y : xs)

inversioni :: Ord a => [a] -> Int
inversioni xs = length (filter (uncurry (>)) (zip xs (tail xs)))



sottoListaPari :: Eq a => [a] ->[a]
sottoListaPari [] = []
sottoListaPari [x] = [x]
sottoListaPari (x : y : xs) = [x] ++ sottoListaPari(xs)

soloPari :: Ord a => [a] -> [a]
soloPari xs = map snd(filter (even.fst) (zip [0..] xs))

soloPariMio :: Ord a => [a] -> [a]
soloPariMio xs = [x | (i , x) <- (zip [0..] xs), i `mod` 2 == 0]

soloDispari :: Ord a => [a] -> [a]
soloDispari xs = map snd(filter (odd.fst) (zip [0..] xs))


sottoListaPariNoRic :: [Int] ->[Int]
sottoListaPariNoRic [] = []
sottoListaPariNoRic xs = filter even xs

sommaListaUltimo :: (Eq a, Num a) => [a] -> Bool
sommaListaUltimo = aux 0
  where
    aux somma [x] = somma == x
    aux somma (x : xs) = aux (somma + x) xs

sommaListaUltimoNoRic :: (Eq a, Num a) => [a] -> Bool
sommaListaUltimoNoRic xs = head ys == sum (tail ys)
  where
   ys = reverse xs

sameSubList :: (Eq a) => [a] -> [a] -> Bool
sameSubList [] _ = True
sameSubList _ [] = False
sameSubList (x : xs) (y : ys) | x == y = sameSubList xs ys
sameSubList (xs) (_ : ys) = sameSubList xs ys


allSubList :: (Eq a) => [a] -> [[a]]
allSubList [] = [[]]
allSubList (x : xs) = xss ++ map (x :) xss
  where
    xss = allSubList xs
    
data Giorno = Lun | Mar | Mer | Gio | Ven | Sab | Dom deriving Show

domani :: Giorno -> Giorno
domani Lun = Mar
domani Mar = Mer
domani Mer = Gio
domani Gio = Ven
domani Ven = Sab
domani Sab = Dom
domani Dom = Lun

sommaPosizioni ::  [Int] -> [Int] -> Int
sommaPosizioni = aux 0
   where
      aux i _ [] = 0
      aux i [] _ = 0
      aux i (x : xs) (y : ys) | i == y = x + aux (i + 1) xs ys
                              | otherwise = aux (i + 1) xs (y : ys)

sommaPosizioniNoRic ::  (Num a, Enum a, Eq a) => [a] -> [a] -> a
sommaPosizioniNoRic xs ys = sum [ x | (i, x) <- (zip [0..] xs ), elem i ys]

fra :: Int -> Giorno -> Giorno
fra x y | x > 1 = fra (x - 1) (domani y)
        | otherwise = domani y 

massimo :: Ord a => [a] -> a
massimo (x : xs) = foldr max x xs

occorrenze :: Eq a => a -> [a] -> Int
occorrenze x xs = length (filter (== x) xs)

occorrenze1 ::Eq a => a -> [a] -> Int
occorrenze1 n xs = length [ x | x <- xs, x == n ]

membro :: Eq a => a -> [a] -> Bool
membro x = foldr (||) False . map ( == x)

membro1 :: Eq a => a -> [a] -> Bool
membro1 n xs = length ([x | x <- xs, x == n] ) >= 1

countUpper :: String -> Int
countUpper = length . filter isUpper . words
  where
    isUpper = all (\c ->c >= 'A' && c <= 'Z')

perfetto :: Int -> Bool
perfetto x = aux 0 x x
 where aux i 0 x = i == (2 * x)
       aux i n x | x `mod` n == 0 = aux (i + n) (n - 1) x
                 | otherwise = aux i (n - 1) x

perfetto2 :: Int -> Bool
perfetto2 x = (2 * x) == sum [ y | x <- [x], y <- [1..x], x `mod` y == 0 ]


mediaMagg :: (Ord a, Integral a) => [(a, a)] -> a
mediaMagg [] = 0
mediaMagg xs = (aux xs) `div` (len xs)
  where
    aux [] = 0
    aux ((x,y) : xs) | x >= 18     = y + aux xs
                     | otherwise  = aux xs

    len [] = 0
    len ((x,y) : xs) | x >= 18     = 1 + len (xs)
                     | otherwise  = len (xs)


mediaMagg2 :: (Ord a, Integral a) => [(a, a)] -> a
mediaMagg2 ((x,y) : xs) = (sum [ y | (x,y) <- xs , x >= 18]) `div` (sum [ 1 | (x,y) <- xs , x >= 18])


sumIndex :: [Int] -> [Int] -> Int
sumIndex _ [] = 0
sumIndex [] _ = 0
sumIndex xs ys = aux 0 xs ys
    where
        aux i (x : xs) (y : ys) | i == y = x + aux (i + 1) xs ys
                                | otherwise = aux (i + 1) xs (y : ys)

prodotto :: Num a => a -> [a] -> a
prodotto _ [] = 0
prodotto n xs = aux 1 n xs
    where
        aux i n [] = 1
        aux i n (x : xs) = (n - x) ^ i * aux (i + 1) n xs

prodotto2 :: Num a => a -> [a] -> a
prodotto2 n xs = product [ (n - x) ^ i | (i, x) <- (zip [1..] xs ), i <= length xs]


es1 :: Ord a => [[a]] -> Maybe [a]
es1  = foldl (aux) Nothing
    where aux Nothing [] = Nothing
          aux Nothing  y = Just y
          aux (Just y) _ = Just y


es2 :: Ord a => [[a]] -> Maybe [a]
es2 [] = Nothing
es2 xs = aux xs Nothing
    where aux [] p       = p
          aux (x : xs) p | (length x) >= 2 = aux xs (Just x)
                         | otherwise       = aux xs p

esame1 :: Ord a => [[a]] -> Maybe [a]
esame1 [] = Nothing
esame1 xs = Just (last (filter (\list -> length list >= 2) xs))


esame2 :: Ord a => a -> [a] -> Maybe a
esame2 _ [] = Nothing
esame2 x (elem : xs) = aux x xs elem
    where
        aux _ [] min = Just min
        aux x (elem : xs) min   | elem < x = aux x xs elem
                                | otherwise = aux x xs min


test :: Ord a => [[a]] -> Maybe [a]
test [] = Nothing
test xs | length (aux xs) >= 1 = Just (head (aux xs))
        | otherwise = Nothing
  where
    aux xs = [x | x <- xs, length x >= 1]

ultimoDispari :: Integral a => [a] -> Maybe a
ultimoDispari [] = Nothing
ultimoDispari xs = aux Nothing xs
  where
      aux p [] = p
      aux p (x : xs) | x `mod` 2 /= 0 = aux (Just x) xs
                     | otherwise = aux p xs

ultimoDispariNoRic :: Integral a => [a] -> Maybe a
ultimoDispariNoRic xs | length (aux xs) >= 1 = Just (head (reverse (aux xs)))
                      | otherwise = Nothing
  where
   aux xs = [ x | x <- xs,  x `mod` 2 /= 0 ]


primoPari :: Integral a => [a] -> Maybe a
primoPari [] = Nothing
primoPari (x : xs) | x `mod` 2 == 0 = Just x
                   | otherwise = primoPari xs

primoPari1 :: Integral a => [a] -> Maybe a
primoPari1 xs | length (aux xs) >= 1 = Just (head (aux xs))
              | otherwise = Nothing
  where
    aux xs = [ x | x <- xs , x `mod` 2 == 0]

inversioni3 :: Ord a => [a] -> Int
inversioni3 xs = length [x | (x,y) <- zip (xs) (tail xs), x < y]

trueList :: [[Bool]] -> [Bool]
trueList xs | aux xs == [] = []
            | otherwise = head (aux xs)
  where
    aux xs = [ x | x <- xs, length x > 2 && foldr (&&) True x]

trueList2 :: [[Int]] -> [Int]
trueList2 xs = aux [] xs
  where
    aux i [] = i
    aux i (x : xs) | length x > 2 = aux x []
                   | otherwise = aux i xs

trueUltimo :: [Int] -> Bool
trueUltimo xs = (sum [x | x <- tail (reverse xs) ]) == head (reverse xs)

massimaLista :: [[a]] -> [[a]]
massimaLista xs = [ x | x <- xs, length x == aux xs]
    where
      aux xs = maximum (map length xs)


sottoListaPari1 :: [a] -> [a]
sottoListaPari1 xs = [ x | (i,x) <- zip [0..] xs, i `mod` 2 == 0]

sottoListaPari1Ric :: [a] -> [a]
sottoListaPari1Ric [] = []
sottoListaPari1Ric xs = aux 0 xs
  where
    aux i [] = []
    aux i (x : xs) | i `mod` 2 == 0 = [x] ++ aux (i + 1) xs
                   | otherwise = aux (i + 1) xs

data Stream a = Elem a (Stream a)

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (Elem x str) = x : takeS (n - 1) str

data List a = Nil | Cons a (List a) deriving Show

lengthList :: List a -> Int
lengthList Nil = 0
lengthList (Cons x xs) = 1 + lengthList xs

lengthList1 ::List a -> Int
lengthList1 xs = length [x | x <- aux xs]
 where
  aux Nil = []
  aux (Cons x xs) = [x] ++ aux xs


convertList :: List a -> [a]
convertList Nil = []
convertList (Cons x xs) = x : convertList xs

reverseConvertList :: [a] -> List a
reverseConvertList [] = Nil
reverseConvertList (x : xs) = (Cons x (reverseConvertList xs))

hexToDecimal :: String -> Int
hexToDecimal xs = sum [ (val x) * 16 ^ y | (x,y) <- zip (reverse xs) [0..]]
  where
    val c | c >= '0' && c <= '9' = ord c - ord '0'
          | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
          | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10

decimalToHex :: Int -> String
decimalToHex n = aux "" n
  where
    aux i 0 = i
    aux i n = aux ([numberToHex (n `mod` 16)] ++ i) (n `div` 16)

    numberToHex n | n < 10 = chr (ord '0' + n)
                  |otherwise = chr (ord 'A' + n - 10)

toUpperCase :: String -> String
toUpperCase [] = []
toUpperCase (x : xs) | x >= 'a' && x <= 'z' = [chr ((ord x) - 32)] ++ toUpperCase xs
                     | otherwise = [x] ++ toUpperCase xs

onlyUpperCase :: [String] -> [String]
onlyUpperCase xs = [ x | x <- xs, length x == length (aux x)]
 where
  aux xs = [ x | x <- xs, x >= 'A' && x <= 'Z']

onlyUpperCase1 :: [String] -> [String]
onlyUpperCase1 [] = []
onlyUpperCase1 [[]] = []
onlyUpperCase1 ((x : xs) : xss) | aux True (x : xs) == True = [(x : xs)] ++ onlyUpperCase1 xss
                                | otherwise = onlyUpperCase1 xss
    where
      aux p [] = p
      aux p (x : xs) | x >= 'A' && x <= 'Z' = aux True xs
                     | otherwise = aux False []


miaInversuione :: [a] -> [a]
miaInversuione [] = []
miaInversuione (x : xs) = miaInversuione xs ++ [x]

invertiUpperCase :: [String] -> [String]
invertiUpperCase [] = []
invertiUpperCase [[]] = []
invertiUpperCase ((x : xs) : xss) | aux True (x : xs) == True = [inv (x : xs)] ++ invertiUpperCase xss
                                  | otherwise = [(x : xs)] ++ invertiUpperCase xss
    where
      aux p [] = p
      aux p (x : xs) | x >= 'A' && x <= 'Z' = aux True xs
                     | otherwise = aux False []

      inv [] = []
      inv (x : xs) = inv xs ++ [x]

invertiUpperCase1 :: [String] -> [String]
invertiUpperCase1 xs = before (head (indexUpper xs)) xs ++ upper xs ++ after (head (reverse (indexUpper xs))) xs
  where
    aux xs = [ x | x <- xs, x >= 'A' && x <= 'Z']
    upper xs = [ reverse x | x <- xs, length x == length (aux x)]
    indexUpper xs = [ i | (i,x) <- zip [0..] xs, length x == length (aux x)]
    before s xs = [ x | (i,x) <- zip [0..] xs, i < s]
    after s xs = [ x | (i,x) <- zip [0..] xs, i > s]

invertiUpperCase2 :: [String] -> [String]
invertiUpperCase2 xs = [ if (aux x) then reverse x else x | x <- xs]
  where
   aux xs = length [ x | x <- xs, x >= 'A' && x <= 'Z'] == length xs



palindroma :: [String] -> [String]
palindroma [] = []
palindroma [[]] = []
palindroma (x : xs) | aux x (reverse x) == True = [x] ++ palindroma xs
                    | otherwise = palindroma xs
  where
    aux [] [] = True
    aux (x : xs) (y : ys) | x == y = True && aux xs ys
                          | otherwise = False

forseDue :: Ord a => [[a]] -> Maybe [a]
forseDue [] = Nothing
forseDue xs = aux Nothing xs
    where
      aux p [] = p
      aux p ((x : xs) : xss) | (length (x : xs)) >= 2 = aux (Just (x : xs)) xss
                     | otherwise = aux p xss



forseDue1 :: Ord a => [[a]] -> Maybe [a]
forseDue1 xs | length (aux xs) > 1 = Just (head (reverse (aux xs)))
            | otherwise = Nothing
        where
          aux xs = [ x | x <- xs, length x >= 2]


piuPiccolo :: Ord a => a -> [a] -> Maybe a
piuPiccolo n xs | length (aux n xs) >= 1 = Just (head (reverse (aux n xs)))
                | otherwise = Nothing
  where
    aux n xs = [ x | x <- xs, x < n]

lastOdd :: Integral a => [a] -> Maybe a
lastOdd [] = Nothing
lastOdd xs = aux Nothing xs
  where
    aux p [] = p
    aux p (x : xs) | x `mod` 2 /= 0 = aux (Just x) xs
                   | otherwise = aux p xs


simulazione :: [Int] -> [Int] -> Int
simulazione xs ys = aux 0 xs ys

  where
    aux i _ [] = 0
    aux i [] _ = 0
    aux i (x : xs) (y : ys) | (contains i (y : ys)) = x + aux (i + 1) xs ys
                            | otherwise = aux (i + 1) xs ys

contains :: Int -> [Int] -> Bool
contains _ [] = False
contains i (x : xs) | i == x = True
                    | otherwise = contains i xs


simulazione1 :: [Int] -> [Int] -> Int
simulazione1 xs ys = sum [ x | (i,x) <- zip [0..] xs, elem i ys]

split :: String -> (String , String)
split [] = ("","")
split xs = ((prima xs), (seconda xs))
  where
   prima "" = ""
   prima (x : xs) | x == ',' = prima ""
                  | otherwise = [x] ++ (prima xs)

   seconda "" = ""
   seconda (x : xs) | x == ',' =  xs ++ seconda ""
                    | otherwise = seconda xs

countUpperWord :: String -> Int
countUpperWord xs = length [ x | x <-  (words xs), length x == length (aux x)]
 where
  aux xs = [ x | x <- xs, x >= 'A' && x <= 'Z']


perfetto6 :: Int -> Bool
perfetto6 0 = False
perfetto6 n = aux 1 n == (2 * n)
  where
   aux i n | n `mod` i == 0 = i + aux (i + 1) n
           | i >= n = 0
           | otherwise = aux (i + 1) n

perfetto62 :: Int -> Bool
perfetto62 n = sum [i | i <- [1..n], n `mod` i == 0] == (2 * n)

minimoSommatoria :: [Int] -> [Int] -> Int
minimoSommatoria xs ys = aux (min (len xs) (len ys)) xs ys
  where
    len [] = 0
    len (x : xs) = 1 + len xs

    min a b | a > b = b
            | otherwise = a

    aux 0 _ _ = 0
    aux i (x : xs) (y : ys) | i == 0 = 0
                            | otherwise = (y * x) + aux (i - 1) xs ys

minimoSommatoria1 :: [Int] -> [Int] -> Int
minimoSommatoria1 xs ys = sum[ x * y | ((i,x),y) <- zip (zip [0..] xs) ys, i < (minimum (length xs, length ys))]

filtro :: (a -> Bool) -> [a] -> [Int]
filtro f [] = []
filtro f xs = aux 0 f xs
  where
    aux i f [] = []
    aux i f (x : xs) | f x == True = [i] ++ aux (i + 1) f xs
                     | otherwise = aux (i + 1) f xs

filtro1 :: (a -> Bool) -> [a] -> [Int]
filtro1 f xs =[ i | (i,x) <- zip [0..] xs, f x == True]

mediaCoppia :: [(Int,Int)] -> Int
mediaCoppia xs = (aux 0 xs) `div` (len xs)
  where
    aux i [] = i
    aux i ((x,y) : xs) | x >= 18 = aux (i + y) xs
                       | otherwise = aux i xs
    len [] = 0
    len ((x,y) : xs) | x >= 18 = 1 + len xs
                     | otherwise = len xs

mediaCoppia1 :: [(Int,Int)] -> Int
mediaCoppia1 xs = sum ([ y | (x,y) <- xs, x >= 18]) `div` length [ y | (x,y) <- xs, x >= 18]

invertiUpperCase3 :: String -> String
invertiUpperCase3 xs = concat (map (++" ") [ if (aux x) then reverse x else x | x <- words xs])
  where
   aux xs = length [ x | x <- xs, x >= 'A' && x <= 'Z'] == length xs

moltiplicaESomma :: Integral a => [a] -> a -> a
moltiplicaESomma [] n = 0
moltiplicaESomma xs n = aux 0 xs n
  where
    aux i [] n = 0
    aux i (x : xs) n = x * n^i + aux (i + 1) xs n

moltiplicaESomma1 :: Integral a => [a] -> a -> a
moltiplicaESomma1 xs n = sum[ x * n^i | (i,x) <- zip [0..] xs]

potenzaDi2Somma :: Integral a => [a] -> a
potenzaDi2Somma [] = 0
potenzaDi2Somma xs = aux 1 xs
  where
    aux i [] = 0
    aux i (x : xs) = 2^(i - 1) * x + aux (i + 1) xs


potenzaDi2Somma1 :: Integral a => [a] -> a
potenzaDi2Somma1 xs = sum [ 2^(i - 1) * x | (i,x) <- zip [1..] xs ]

moltiplicaBig :: Integral a => a -> [a] -> a
moltiplicaBig n xs = aux 1 n xs
  where
    aux i n [] = 1
    aux i n (x : xs) = (n - x)^i * aux (i + 1) n xs

moltiplicaBig1 :: Integral a => a -> [a] -> a
moltiplicaBig1 n xs = product [ (n - x)^i | (i,x) <- zip [1..] xs]

stringheCoppie :: [Either a Bool] -> Maybe a
stringheCoppie xs | length (foo xs) > 0 = Just (head (reverse (foo xs)))
                  | otherwise = Nothing
  where
    foo xs = [ x | (i, Left x) <- zip [0..] xs, aux (i + 1) xs == True]
    aux index xs = head [ x | (i, Right x) <- zip [0..] xs, i == index]

stringheCoppie1 :: [Either a b] -> [a]
stringheCoppie1 [] = []
stringheCoppie1 (Left x : xs) = [x] ++ stringheCoppie1 xs
stringheCoppie1 (Right x : xs) = stringheCoppie1 xs



minOdd :: [Int] -> Maybe (Int,Int)
minOdd xs | length (foo xs) > 0 = Just (minimum (foo xs))
          | otherwise = Nothing
  where
   foo xs = [(x,y) | x <- (aux xs), y <- (aux xs), (test x y xs) == True]
   aux xs = [ x | x <- xs, odd x]
   occ n xs = length [ x | x <- xs, n == x]
   test n t xs | n == t && (occ n xs) > 1 = True
               | n /= t = True
               | otherwise = False

sommaCoppie :: [Int] -> [(Int,Int)]
sommaCoppie xs = aux 0 xs
  where
    aux i [] = []
    aux i (x : xs) = [(x , (somma 0 i xs))] ++ aux (i + 1) xs

    somma n i [] = 0
    somma n i (x : xs) | i >= n = x + somma n (i + 1) xs
                       | otherwise = somma n (i + 1) xs

sommaCoppie1 :: [Int] -> [(Int,Int)]
sommaCoppie1 xs = [(x,(aux i xs)) | (x,i) <- zip xs [0..] ]
  where
    aux index xs = sum [ x | (x,i) <- zip xs [0..], i > index ]

-----------------------------------------------------------------------------------------------------------------------

esameEs2 :: (Eq a, Enum a) => [a] -> [a] -> Bool
esameEs2 xs ys = aux False xs ys
  where
    aux p _ [] = p
    aux p [] _ = p
    aux p (x : xs) (y : ys) | (test False x (y : ys) ) == True = aux True [] []
                            | otherwise = aux p xs (y : ys)

    test p x [] = p
    test p x (y : ys) | (x) == (succ y) = test True x []
                      | otherwise = test p x ys




esameEs1 :: (Eq a, Enum a) => [a] -> [a] -> Bool
esameEs1 xs ys = length (aux xs ys) >= 1
  where
    aux xs ys = [ x | (x,y) <- zip xs ys, elem (succ y) xs]





successore4 :: (Eq a, Num a) => [a] -> [a] -> Bool
successore4 l1 l2 = any (uncurry aux)(zip l1 l2)
      where
        aux x y | succ y == x = True
                | otherwise  = False