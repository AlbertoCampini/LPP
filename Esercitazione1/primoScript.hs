anno_terra :: Float
anno_terra = 2 * pi * 150e6

v_terra :: Float
v_terra = anno_terra / (365 * 24)

anno_mercurio :: Float
anno_mercurio = 2 * pi * 58e6

v_mercurio :: Float
v_mercurio = anno_mercurio / (88 * 24)

area :: Float -> Float
area n = n * pi * 2

somma_numeri :: Int -> Int
somma_numeri n = (n * (n+1)) `div` 2

bisestile :: Int -> Bool
bisestile n = if (((((n `mod` 4) == 0) && (n `mod` 100) /= 0)) || (n `mod` 400) == 0) then True else False

scambia :: (Int, Int) -> (Int, Int)
scambia (x,y) = (y,x)

primo :: Integral a => a -> Bool
primo n = [ d | d <- [2..n], n `mod` d == 0 ] == [n]

test :: Enum a => a -> [a]
test n = n : test (succ n)

primi :: Int -> [Integer]
primi n = take n (filter primo (test 2))

primoMaggioreDi :: Integer -> Integer
primoMaggioreDi n = head (take 1 (filter primo (test (n + 1))))


data Puntocardinale = Nord | Sud | Est | Ovest deriving Show 

sinistra :: Puntocardinale -> Puntocardinale
sinistra Nord = Est
sinistra Sud = Ovest
sinistra Ovest = Nord
sinistra Est = Sud

destra :: Puntocardinale -> Puntocardinale
destra = indietro . sinistra

indietro :: Puntocardinale -> Puntocardinale
indietro = sinistra . sinistra

data Giorno = Lun | Mar | Mer | Gio | Ven | Sab | Dom deriving show
domani :: Giorno -> Giorno
domani Lun = Mar
domani Mar = Mer
domani Mer = Gio
domani Gio = Ven
domani Ven = Sab
domani Sab = Dom
domani Dom = Lun

fra :: Int -> Giorno -> Giorno

data ForseInt = Niente | Proprio Int deriving Show

totale :: [Int] -> ForseInt
totale 0 (x : _)  = Proprio x
totale _ _        = Niente

data Numero = F f | I n deriving show

somma :: Numero -> Numero -> Numero
somma (I n) (I m) = I(m + n)
somma (I n) (F m) = F(FromIntegral m + n)
somma (F n) (I m) = F(m + FromIntegral n)
somma (F n) (F m) = F(m + n)







