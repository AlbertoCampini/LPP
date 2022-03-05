inversioni1 :: Ord a => [a] -> Int
inversioni1 [] = 0
inversioni1 [x] = 0
inversioni1 (x : y : list)  | x < y = 1 + inversioni1 (y : list)
                            | otherwise = inversioni1 (y : list)

inversioni2 :: Ord a => [a] -> Int
inversioni2 list = length (filter (uncurry (<)) (zip list (tail list)))

inversioni3 :: Ord a => [a] -> Int
inversioni3 xs = length [x | (x,y) <- zip (xs) (tail xs), x < y]