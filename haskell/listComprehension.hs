map :: (a -> b) -> [a] -> [b]
map f list = [ f x | x <- list ] 

filter :: (a -> Bool) -> [a] -> [a]
filter f list = [ x | x <- list, f x ]


{-
primo :: Integral a => a -> Bool
primo n = [ d | d <- [2..n], n `mod` d == 0 ] == [n]
-}
primo :: Integral a => a -> Bool 
primo a = null [ x | x <- [2 .. a - 1], a `mod` x == 0 ]

terne :: Int -> [(Int, Int, Int)]
terne n = [ (a, b, c) | a <- [1..n]
                      , b <- [a + 1..n]
                      , coprimi a b
                      , c <- [b + 1..n]
                      , a^2 + b^2 == c^2 ]
    where
        -- trova il mcd di a e b e controlla che sia uguale a 1
        coprimi a b = mcd a b == 1

        mcd 0 n = n
        mcd m n | m > n = mcd n m
        mcd m n = mcd m (n - m)