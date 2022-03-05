guardia :: Int -> Int 
guardia n   | n `mod` 2 == 0 = n + 1
            | otherwise = abs n

condizione :: Int -> Int 
condizione n =  if n `mod` 2 == 0 
                then n + 1
                else abs n

bisestile :: Int -> Bool
bisestile n = ((n `mod` 4 == 0) && (n `mod` 100 /= 0)) || (n `mod` 400 == 0)

giorni :: Int -> Int 
giorni n    | bisestile n = 366
            | otherwise = 365