import Data.Char (ord, chr)

showHex :: Int -> String
showHex n   | n < 0     = '-' : showHex (negate n)
            | n < 16    = [aux n]
            | otherwise = showHex (n `div` 16) ++ [aux (n `mod` 16)]
    where
        aux n   | n < 10    = chr (ord '0' + n)
                | otherwise = chr (ord 'A' + n - 10)

readHexRic :: String -> Int
readHexRic = aux 0
    where
        aux res [] = res
        aux res (c : cs) = aux (res * 16 + readChar c) cs

        readChar c  | c >= '0' && c <= '9' = ord c - ord '0'
                    | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
                    | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10

readHex :: String -> Int
readHex = foldl (\res c -> res * 16 + readChar c) 0
    where
        readChar c  | c >= '0' && c <= '9' = ord c - ord '0'
                    | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
                    | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10