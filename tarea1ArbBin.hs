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

-- Binary Tree will be defined
data ArbBinario = Hoja
                | N ArbBinario ArbBinario 
                | N1 ArbBinario deriving (Eq, Show) 

-- Function that creates binary tree with any size and a node
-- Following algorithms and data structures II 
construirArbTree :: Integer -> Integer -> ArbBinario
construirArbTree tamano nodo
 | (2*nodo) + 1 <= tamano = N (construirArbTree tamano (2*nodo)) (construirArbTree tamano ((2*nodo) + 1))
 | 2*nodo <=  tamano = N1 (construirArbTree  tamano (2*nodo))
 | otherwise = Hoja

-- Function that calculates binary tree's size 
calcularTamano :: ArbBinario  -> Integer
calcularTamano Hoja = 1
calcularTamano (N arbI arbD) = 1 + (calcularTamano arbI)+ (calcularTamano arbD)
calcularTamano (N1 arb) = 1 + (calcularTamano arb)

-- Function that calculate binary tree's depth
calcularProfundidad :: ArbBinario -> Integer
calcularProfundidad Hoja = 1
calcularProfundidad (N arb1 arb2) = 1 + max (calcularProfundidad arb1) (calcularProfundidad arb2)
calcularProfundidad (N1 arb) = 1 + calcularProfundidad arb

-- Principal Function that creates a binary tree of any size
hacerABinTree :: Integer -> ArbBinario
hacerABinTree tamano
 | otherwise = construirArbTree tamano 1
