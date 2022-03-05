putStrLn2 :: String -> IO ()
putStrLn2 = foldr ((>>) . putChar) (putChar '\n')

putLines :: [String] -> IO ()
putLines = foldr ((>>) . putStrLn) (return ())

getLines :: IO [String]
getLines = getLine >>= \line ->
            if line == "" then return []
            else getLines >>= \lines -> return (line : lines)

getInt :: IO Int
getInt = getLine >>= return . read

somma :: IO ()
somma = getInt >>= aux 0
  where
    aux res 0 = putStrLn (show res)
    aux res n = getInt >>= \k -> aux (res + k) (n - 1)