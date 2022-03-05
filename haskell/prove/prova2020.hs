{-
Domanda 8
Definire una funzione che, applicata a una lista xs, restituisca la sotto-lista 
contenente tutti e soli gli elementi di xs in posizione pari, nello stesso ordine 
in cui compaiono in xs e assumendo che il primo elemento della lista si trovi in 
posizione 0. È vietato l’uso esplicito della ricorsione, ma si possono usare 
tutte le funzioni definite nel modulo Prelude.
-}
listaPariLib :: [a] -> [a]
listaPariLib = map snd . filter (\(x, y) -> even x) . zip [0..]

{-
Domanda 9
Definire una funzione inversioni che, applicata a una lista xs, calcoli il numero 
di inversioni di xs, ovvero il numero di elementi di xs immediatamente seguiti da 
un elemento più piccolo. È vietato fare uso esplicito della ricorsione, ma si 
possono usare tutte le funzioni definite nel modulo Prelude.
-}
inversioniLib :: Ord a => [a] -> Int
inversioniLib xs = length (filter (uncurry (>)) (zip xs (tail xs)))

{-
Domanda 10
Definire una funzione che, applicata a una lista xs, restituisca la sotto-lista 
contenente tutti e soli gli elementi di xs in posizione pari, nello stesso ordine 
in cui compaiono in xs e assumendo che il primo elemento della lista si trovi in 
posizione 0. È vietato fare uso di funzioni della libreria standard ad eccezione 
di mod e quelle che hanno un nome simbolico, come +, ., ecc.
-}
listaPariRic :: [a] -> [a]
listaPariRic [] = []
listaPariRic [x] = [x]
listaPariRic (x : y : xs) = x : listaPariRic xs

{-
Domanda 11
Definire una funzione inversioni che, applicata a una lista xs, calcoli il numero 
di inversioni di xs, ovvero il numero di elementi di xs immediatamente seguiti da 
un elemento più piccolo. È vietato fare uso di funzioni della libreria standard ad 
eccezione di mod e quelle che hanno un nome simbolico, come +, ., ecc. Fare in modo 
che inversioni abbia il tipo più generale possibile.
-}
inversioniRic :: Ord a => [a] -> Int
inversioniRic [] = 0
inversioniRic [x] = 0
inversioniRic (x : y : xs)  | x > y = 1 + inversioniRic (y : xs)
                            | otherwise = inversioniRic (y : xs)

{-
Domanda 12
Dato il tipo algebrico
data Tree a = Empty | Node a [Tree a]
per rappresentare alberi n-ari, definire una funzione
elements :: Tree a -> [a]
che calcoli la lista di tutti gli elementi contenuti nell’albero in un ordine a scelta. 
Usare la ricorsione solo laddove necessario, sfruttando il più possibile le funzioni 
del modulo Prelude. Se opportuno, è ammessa la definizione di funzioni ausiliarie.
-}
data Tree a = Empty | Node a [Tree a]

elements :: Tree a -> [a]
elements Empty = []
elements (Node a t) = a : concat (map elements t)

{-
Domanda 13
Dato il tipo algebrico
data Tree a = Empty | Node a [Tree a]
per rappresentare alberi n-ari, diciamo che un albero è in forma normale se è Empty 
oppure se è costruito senza usare Empty. Definire una funzione
normalize :: Tree a -> Tree a
che trasformi un albero in forma normale, usando la ricorsione solo laddove è necessario 
e sfruttando il più possibile le funzioni del modulo Prelude. Se opportuno, è ammessa 
la definizione di funzioni ausiliarie.
-}
normalize :: Tree a -> Tree a
normalize Empty = Empty
normalize (Node a tree) = Node a (concat (map (aux . normalize) tree))
    where
        aux Empty = []
        aux tree = [tree]