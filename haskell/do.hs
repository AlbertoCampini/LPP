for :: [a] -> (a -> IO ()) -> IO ()
for []       _ = return ()
for (x : xs) f = do f x
                    for xs f

putStrLn2 :: String -> IO ()
putStrLn2 str = do  for str putChar
                    putChar '\n'

getLines :: IO [String]
getLines = do   line <- getLine
                if null line then return []
                else do lines <- getLines
                        return (line : lines)
