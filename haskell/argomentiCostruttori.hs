data ForseInt = Niente | Proprio Int
    deriving Show

testa :: [Int] -> ForseInt
testa [] = Niente
testa (x : xs) = Proprio x

data Numero = I Int | F Float 
    deriving Show 
    
somma :: Numero -> Numero -> Numero
somma (I x) (I y) = I (x + y)
somma (F x) (F y) = F (x + y)
somma (F x) (I y) = F (x + fromIntegral y)
somma (I x) (F y) = F (fromIntegral x + y)

sommatoria :: [Numero] -> Numero
sommatoria = foldr somma (I 0)

proprio :: [ForseInt] -> [Int]
proprio [] = []
proprio (Niente : xs) = proprio xs
proprio (Proprio x : xs) = x : proprio xs