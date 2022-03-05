pospari1 :: [a] -> [a]
pospari1 [] = []
pospari1 (x : y : list) = x : pospari1 list
pospari1 [x] = [x]

pospari2 :: [a] -> [a]
pospari2 = map snd . filter (\(x, y) -> even x) . zip [0 ..]