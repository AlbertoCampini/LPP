import Data.Char (ord, chr)

maybeLength :: Maybe a -> Int
maybeLength Nothing = 0
maybeLength (Just _) = 1

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

maybeFilter :: (a -> Bool) -> Maybe a -> Maybe a
maybeFilter p (Just x) | p x = Just x
maybeFilter _ _ = Nothing

somma :: Either Int Float -> Either Int Float -> Either Int Float
somma (Left x) (Left y) = Left (x + y)
somma (Right x) (Right y) = Right (x + y)
somma (Right x) (Left y) = Right (x + fromIntegral y)
somma (Left x) (Right y) = Right (fromIntegral x + y)

data List a = Nil | Cons a (List a)
lengthNC :: List a -> Int
lengthNC Nil = 0
lengthNC (Cons _ list) = 1 + lengthNC list

data Stream a = Elem a (Stream a) deriving Show

foreverS :: a -> Stream a
foreverS n = Elem n (foreverS n)

fromS :: Enum a => a -> Stream a
fromS n = Elem n (fromS (succ n))

takeEvenN :: Int -> Stream Int -> [Int]
takeEvenN 0 _ = []
takeEvenN n (Elem x xs) | x `mod` 2 == 0 = [x] ++ takeEvenN (n - 1) xs
                        | otherwise = takeEvenN (n) xs

takeS :: Int -> Stream a -> [a]
takeS 0 _ = []
takeS n (Elem x str) = x : takeS (n - 1) str

readHex :: String -> Int
readHex "" = 1
readHex xs = aux ((len xs) - 1) xs
  where
    aux i [] = 0
    aux i (x : xs) = val x * (16^i) + aux (i - 1) xs

    val c | c >= '0' && c <= '9' = ord c - ord '0'
         | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
         | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10

    len [] = 0
    len (x : xs) = 1 + len xs


readHex1 :: String -> Int
readHex1 xs = sum [val x * (16 ^ i) | (i,x) <- zip [0..] (reverse xs) ]
  where
    val c | c >= '0' && c <= '9' = ord c - ord '0'
             | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
             | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
