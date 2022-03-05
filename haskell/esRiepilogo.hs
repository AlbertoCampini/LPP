import System.IO (isEOF)

union :: Ord a => [a] -> [a] -> [a]
union [] [] = []
union xs [] = xs
union [] ys = ys
union (x : xs) (y : ys) | x < y = x : union xs (y : ys)
                        | x > y = y : union (x : xs) ys
                        | otherwise = x : union xs ys

intersection :: Ord a => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection (x : xs) (y : ys)  | x == y = x : intersection xs ys
                                | x < y = intersection xs (y : ys)
                                | x > y = intersection (x : xs) ys

difference :: Ord a => [a] -> [a] -> [a]
difference xs [] = xs
difference [] _ = []
difference (x : xs) (y : ys)    | x < y = x : difference xs (y : ys)
                                | x > y = difference (x : xs) ys
                                | otherwise = difference xs ys

main :: IO ()
main = do s <- getContents
          putStrLn $
            " " ++ (show $ length $ lines s) ++
            " " ++ (show $ length $ words s) ++
            " " ++ (show $ length s)