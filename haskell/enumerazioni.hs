data PuntoCardinale = Nord | Sud | Ovest | Est
    deriving Show

sinistra :: PuntoCardinale -> PuntoCardinale
sinistra Nord  = Est
sinistra Est   = Sud
sinistra Sud   = Ovest
sinistra Ovest = Nord

destra :: PuntoCardinale -> PuntoCardinale
destra = indietro . sinistra

indietro :: PuntoCardinale -> PuntoCardinale
indietro = sinistra . sinistra

data Giorno = Lun | Mar | Mer | Gio | Ven | Sab | Dom
    deriving (Show, Enum)

domani :: Giorno -> Giorno
domani Dom = Lun
domani g = succ g

{-
fra :: Int -> Giorno -> Giorno
fra 0 = id
fra n = domani . fra (n - 1)
-}
fra :: Int -> Giorno -> Giorno
fra 0 g = g
fra n g = fra (n - 1) (domani g)

-- crea una lista di funzioni "domani" lunga n che poi concatena con .
fraReplicate :: Int -> Giorno -> Giorno
fraReplicate n = foldr (.) id (replicate n domani)