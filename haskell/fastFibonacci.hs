type Matrice = (Integer, Integer, Integer, Integer)

mul :: Matrice -> Matrice -> Matrice
mul (a11, a12, a21, a22) (b11, b12, b21, b22) =
  (a11 * b11 + a12 * b21,
   a11 * b12 + a12 * b22,
   a21 * b11 + a22 * b21,
   a21 * b12 + a22 * b22)

pow :: Matrice -> Int -> Matrice
pow a k | k == 0         = (1, 0, 0, 1)
        | k `mod` 2 == 0 = b `mul` b
        | otherwise      = a `mul` b `mul` b
  where
    b = a `pow` (k `div` 2)

fibonacci :: Int -> Integer
fibonacci k = risultato
  where
    (_, risultato, _, _) = (1, 1, 1, 0) `pow` k