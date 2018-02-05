-- Universidad Simón Bolívar
-- CI3661: Programming Languages I
--
-- Tarea 1 Haskell
--
-- Authors:
--
-- David Cabeza <13-10191@usb.ve>
-- Fabiola Martínez <13-10838@usb.ve>
--
-- February, 2018.

-- Definimos el arbol binario
data ArbBinario = Hoja
                | N ArbBinario ArbBinario 
                | N1 ArbBinario deriving (Eq, Show) 

-- Funcion que crea arbol binario dado el tamano  
hacerABinTree :: Integer -> Integer -> ArbBinario
hacerABinTree tamano nodo
 | (2*nodo) + 1 <= tamano = N (hacerABinTree tamano (2*nodo)) (hacerABinTree tamano ((2*nodo) + 1))
 | 2*nodo <=  tamano = N1 (hacerABinTree  tamano (2*nodo))
 | otherwise = Hoja

-- Funcion que calcula el tamano de un arbol binario
calcularTamano :: ArbBinario  -> Integer
calcularTamano Hoja = 1
calcularTamano (N arbI arbD) = 1 + (calcularTamano arbI)+ (calcularTamano arbD)
calcularTamano (N1 arb) = 1 + (calcularTamano arb)

-- Funcion que calcula la profund de un arbol binario
calcularProfundidad :: ArbBinario -> Integer
calcularProfundidad Hoja = 1
calcularProfundidad (N arb1 arb2) = 1 + max (calcularProfundidad arb1) (calcularProfundidad arb2)
calcularProfundidad (N1 arb) = 1 + calcularProfundidad arb
