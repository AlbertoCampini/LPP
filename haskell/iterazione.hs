{-
public static int fibonacci(int k) {
    assert k >= 0;
    int m = 0;
    int n = 1;
    while (k > 0) {
        n = n + m;
        m = n - m;
        k = k - 1;
    }
    return m;
}
-}

fibonacci :: Int -> Integer
-- uso richiamo ausiliaria per "tradurre" valorizzazione di m e n
fibonacci = aux 0 1
  where
    aux m _ 0 = m
    {-
    essendo che n = m + n, allora m = n - m del while viene 
    espresso come m = (m + n) - m, (+) m e - m si annullano,
    quindi nel "prossimo" ciclo di ricorsione m = n
    -}
    aux m n k = aux n (m + n) (k - 1)

{-
public static boolean primo(int n) {
    assert n >= 0;
    int k = 2;
    while (k < n && n % k != 0) k++;
    return k == n;
}
-}

primo :: Int -> Bool
primo n = aux 2
  where
    aux k | k >= n         = k == n
          | n `mod` k == 0 = False
          | otherwise      = aux (k + 1)

{-
public static int fattoriale(int n) {
    assert n >= 0;
    int res = 1;
    while (n > 0) {
       res = res * n;
       n = n - 1;
    }
    return res;
}
-}

fattoriale :: Int -> Int 
fattoriale = aux 1
    where
        aux res 0 = res 
        aux res n = aux (res * n) (n - 1)

{-
public static int bits(int n) {
    assert n >= 0;
    int bits = 0;
    while (n > 0) {
        bits = bits + n % 2;
        n = n / 2;
    }
    return bits;
}

soluzione slide
bits :: Int -> Int
bits = aux 0
  where
    aux bits 0 = bits
    aux bits n = aux (bits + n `mod` 2) (n `div` 2)
-}

bits :: Int -> Int
bits = aux 0
    where
        aux b n | n == 0 = b
                | otherwise = aux (b + n `mod` 2) (n `div` 2)


{-
public static int euclide(int m, int n) {
    assert m > 0 && n > 0;
    while (m != n)
        if (m < n) n -= m; else m -= n;
    return n;
}
-}

euclide :: Int -> Int -> Int 
euclide m n | m == n = n
            | m < n = euclide m (n - m)
            | otherwise = euclide (m - n) n 