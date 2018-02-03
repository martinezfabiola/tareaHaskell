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

-- duplicarCadaOtro :: [Integer] -> [Integer]
-- duplicarCadaOtro = [x*2 | (index)]