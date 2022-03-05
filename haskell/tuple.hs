scambia :: (Int, Int) -> (Int, Int)
scambia (x, y) = (y, x)

ordina :: (Int, Int, Int) -> (Int, Int, Int)
ordina (x, y, z)    | x > y = ordina (y, x, z)
                    | y > z = ordina (x, z, y)
                    | otherwise = (x, y, z)

sommaComplesso :: (Double , Double) -> (Double, Double) -> (Double, Double)
sommaComplesso (realeA, iA) (realeB, iB) = (realeA + realeB, iA + iB) 

negateComplesso :: (Double, Double) -> (Double, Double)
negateComplesso (reale, i) = (negate reale, i)

sottrazioneComplesso :: (Double , Double) -> (Double, Double) -> (Double, Double)
sottrazioneComplesso cA cB = sommaComplesso cA (negateComplesso cB)