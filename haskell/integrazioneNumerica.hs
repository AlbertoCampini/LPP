{-
public static double trapezi(Function<Double, Double> f,
                            double a, double b, int n) {
double area = 0;
double h = (b - a) / n;
while (n > 0) {
    area += (f.apply(a) + f.apply(a + h)) * h / 2;
    a += h;
    n--;
}
return area;
}
-}

trapeziRic :: (Double -> Double) -> Double -> Double -> Int -> Double
trapeziRic f a b n = aux 0 a n
  where
    aux area _ 0 = area
    aux area a n = aux (area + (f a + f (a + h)) * h / 2) (a + h) (n - 1)

    h = (b - a) / fromIntegral n

{-
public static void main(String... args) {
    System.out.println(trapezi(x -> x / 2 + Math.sin(2 * x), 1, 7, 3));
}
-}

-- trapeziRic (\x -> x / 2 + sin (2 * x)) 1 7 3)

trapezi :: (Double -> Double) -> Double -> Double -> Int -> Double
trapezi f a b n = sum as
    where
        zs = map (\i -> f (a + fromIntegral i * h)) [0..n]
        as = map (\(x, y) -> (x + y) * h / 2) (zip zs (tail zs))
        h  = (b - a) / fromIntegral n

match :: Eq a => [a] -> [a] -> Bool
match xs ys = any (uncurry (==)) (zip xs ys)

adiacenti :: Eq a => [a] -> Bool
adiacenti list = any (uncurry (==)) (zip list (tail list))

polinomio :: [Float] -> Float -> Float
polinomio cs x = sum (map (uncurry (*)) (zip cs (map (x ^) [0 ..])))

{-
perfetto :: Int -> Bool
perfetto n = n == sum (filter ((== 0) . (n `mod`)) [1 .. n - 1])
-}
perfetto :: Int -> Bool
perfetto n = n == sum (filter (\x -> n `mod` x == 0) [1 .. n - 1])

ordinata :: Ord a => [a] -> Bool
ordinata list = all (uncurry (<=)) (zip list (tail list))