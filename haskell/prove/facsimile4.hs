bigSomma1 :: Num a => [a] -> a
bigSomma1 = aux 0
    where
        aux _ [] = 0
        aux n (x : xs) = 2^n * x + aux (n + 1) xs

bigSomma2 :: Num a => [a] -> a
bigSomma2 = sum . map (\(x, y) -> 2^x * y) . zip [0 ..]