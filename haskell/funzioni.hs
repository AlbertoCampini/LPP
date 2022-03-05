dispari :: Int -> Bool
dispari n = n `mod` 2 /= 0

bisestile :: Int -> Bool
bisestile n = ((n `mod` 4 == 0) && (n `mod` 100 /= 0)) || (n `mod` 400 == 0)

somma :: Int -> Int
somma n = (n * (n + 1)) `div` 2

{-
^ potenza per numeri interi
^^ potenza numeri razionali
** potenza numeri float
-}
area :: Float -> Float
area n = pi * (n ** 2)
