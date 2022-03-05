
primo :: Int -> Bool
primo n = aux 2
    where
        aux k   | k >= n         = k == n
                | n `mod` k == 0 = False
                | otherwise      = aux (k + 1)

{-
primi :: Int -> [Int]
primi n = aux 2
  where
    aux k | k > n     = []
          | primo k   = k : aux (k + 1)
          | otherwise = aux (k + 1)
-}
listaPrimi :: Int -> [Int]
listaPrimi lung = aux 2 []
    where
        aux n list  | length list == lung = list
                    | primo n = aux (n + 1) (list ++ [n])
                    | otherwise = aux (n + 1) list

media :: [Int] -> Float
media list = fromIntegral (sum list) / fromIntegral (length list)

fattorialeNat :: Int -> Int
fattorialeNat nat = product [1 .. nat]

intervallo :: Int -> Int -> [Int]
intervallo m n  | m > n = []
                | otherwise = m : intervallo (m + 1) n