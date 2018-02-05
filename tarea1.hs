-- Universidad Simón Bolívar
-- CI3661: Programming Languages I
--
-- Activity 1 Haskell
--
-- Authors:
--
-- David Cabeza <13-10191@usb.ve>
-- Fabiola Martínez <13-10838@usb.ve>
--
-- February, 2018.

aDigitos :: Integer -> [Integer]
aDigitos numTarjeta
-- numTarjeta is 0 or is negative then return []
  | numTarjeta > 0 = aDigitos (numTarjeta `div` 10) ++ [numTarjeta `mod` 10]
  | otherwise = []

aDigitosRev :: Integer -> [Integer]
aDigitosRev = reverse . aDigitos

duplicarCadaOtro :: [Integer] -> [Integer]
duplicarCadaOtro [] = []
duplicarCadaOtro (primerDigito:[]) = [primerDigito]
duplicarCadaOtro (primerDigito:segundoDigito:restoDigitos) = (primerDigito*2):segundoDigito:duplicarCadaOtro restoDigitos

sumDigitos :: [Integer] -> Integer
sumDigitos [] = 0
sumDigitos (p:resto) = p + sumDigitos resto

validate :: Integer -> Bool
validate tarjetadeCredito
  | (mod (sumDigitos (duplicarCadaOtro (aDigitosRev tarjetadeCredito))) 10 == 0) = True
  | otherwise = False
-- validate tarjetadeCredito = sumDigitos (duplicarCadaOtro (aDigitosRev tarjetadeCredito)) `mod` 10 == 0

data ArbBinario = L | N BinTree Bintree deriving (Eq, Show)

-- Crea arbol binario completo de tamano 2(n+1)-1
crearArbBinario 0 = L 
crearArbBinario n = N (crearArbBinario(n-1))(crearArbBinario(n-1))

-- Calcula el tamano del arbol 
tam L = 1
tam (N t1 t2) = 1 + tam t1 + tam t2


