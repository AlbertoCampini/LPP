massimo :: Int -> Int -> Int
massimo x y | x >= y = x
            | otherwise = y

minimo :: Int -> Int -> Int
minimo x y  | x >= y = y
            | otherwise = x

potenza :: Int -> Int -> Int
potenza _ 0 = 1
potenza m n = m * potenza m (n - 1)

pow2 :: Int -> Int
pow2 = potenza 2