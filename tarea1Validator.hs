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

---- Exercise one: luhn's algorithm

-- Function that puts into an array a credit card number
aDigitos :: Integer -> [Integer]
aDigitos x
  -- x is 0 or is negative then return []
  | x > 0 = aDigitos (x `div` 10) ++ [x `rem` 10]
  | otherwise = []

-- Function that puts into an array a credit card number but in reverse
aDigitosRev :: Integer -> [Integer]
aDigitosRev = reverse . aDigitos

-- Function that duplicates some digits of the credit card number
duplicarCadaOtro :: [Integer] -> [Integer]
duplicarCadaOtro [] = []
duplicarCadaOtro (x:[]) = [x]
duplicarCadaOtro (x:y:xs) = x:(y*2):duplicarCadaOtro xs

-- Function that separates a number in digit of any array 
separarDigitos :: [Integer] -> [Integer]
separarDigitos [] = []
separarDigitos (x:[]) = if x < 10 then [x] else aDigitos x
separarDigitos (x:xs) = (if x < 10 then [x] else aDigitos x) ++ separarDigitos xs

--- Function that adds up to the elements of an array
sumDigitos :: [Integer] -> Integer
sumDigitos x = sum x 

-- Main function that verifies if a credit card number is valid
validate :: Integer -> Bool
validate numTarjetaCredito
  | (numTarjetaCredito < 0) = False
  | otherwise = (sumDigitos (separarDigitos (duplicarCadaOtro (aDigitosRev numTarjetaCredito))) `rem` 10 == 0)
