fattoriale :: Int -> Int
fattoriale 0 = 1
fattoriale n = n * fattoriale (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

sommaNaturali :: Int -> Int 
sommaNaturali 0 = 0
sommaNaturali n = n + sommaNaturali (n - 1)

pow2 :: Int -> Int 
pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)

bits :: Int -> Int
bits 0 = 0
 -- si potrebbe usare even
bits n  | n `mod` 2 == 0 = 0 + bits (n `div` 2)
        | otherwise = 1 + bits (n `div` 2)

potenzaDi2 :: Int -> Bool
potenzaDi2 0 = False
potenzaDi2 1 = True
 -- si potrebbe usare even
potenzaDi2 n = n `mod` 2 == 0 && potenzaDi2 (n `div` 2)

{-
pari
\x -> x `mod` 2 == 0

dispari
\x -> x `mod` 2 /= 0

val assoluto
\x -> if x >= 0 then x else negate x
-}