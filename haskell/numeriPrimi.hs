primo :: Integral a => a -> Bool
primo n = [ d | d <- [2..n], n `mod` d == 0 ] == [n]

primi :: Int -> [Integer]
primi n = take n (filter primo (enumFrom 2))

{-
primoMaggioreDi :: Integer -> Integer
primoMaggioreDi n = head (filter (> n) (filter primo (enumFrom 2)))

-- OPPURE

primoMaggioreDi :: Integer -> Integer
primoMaggioreDi n = head (filter primo (enumFrom (max 2 (n + 1))))
-}
primoMaggioreDi :: Integer -> Integer
primoMaggioreDi n = head [ x | x <- [2 ..], primo x, x > n ]

{-
primiGemelli :: Int -> [(Integer, Integer)]
primiGemelli n = take n (filter gemelli (zip ps (tail ps)))
  where
    ps = filter primo (enumFrom 2)
    gemelli (p, q) = q == p + 2
-}
primiGemelli :: Int -> [(Integer, Integer)]
primiGemelli n = take n (filter (\(x, y) -> y - x == 2) (zip listaPrimi (tail listaPrimi)))
    where
        listaPrimi = filter primo (enumFrom 2)