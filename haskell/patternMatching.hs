prodotto :: [Int] -> Int
prodotto [] = 1
prodotto (testa : coda) = testa * prodotto coda

inverti :: [Int] -> [Int]
inverti [] = []
inverti (testa : coda) = inverti coda ++ [testa]

sommaCongiunta :: [Int] -> [Int] -> [Int]
sommaCongiunta _ [] = []
sommaCongiunta [] _ = []
sommaCongiunta (x : xs) (y : ys) = (x + y) : sommaCongiunta xs ys