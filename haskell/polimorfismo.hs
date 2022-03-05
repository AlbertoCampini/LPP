{-
map
input:
- funzione a -> b
- lista con valori di tipo a
output:
- lista con valori di tipo b
esegue la funzione su ogni valore della lista

filter
input:
- condizione parziale a -> Bool
- lista con valori di tipo a
output:
- lista con valori di tipo a
crea lista con valori che rispettano condizione

foldr
input:
- condizione o funzione che esegue tra i valori della lista a -> b -> b
- valore di ritorno di "caso base"/fine lista tipo b
- lista con valori di tipo b
output:
- valore ottenuto da condizione/funzione applicata a tutti gli elementi della lista di tipo b
esegue operazioni/funzioni su tra tutti gli elementi della lista
-}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x [] = x
foldr f x (y : ys) = f y (foldr f x ys)

concat :: [[a]] -> [a]
concat = foldr (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (||) False . map p

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (&&) True . map p

massimo :: Ord a => [a] -> a
massimo (x : list) = foldr max x list

occorrenze :: Eq a => a -> [a] -> Int
occorrenze n = length . filter (== n)

membro :: Eq a => a -> [a] -> Bool
-- al primo True any interrompe esecuzione
membro x = Main.any (== x)