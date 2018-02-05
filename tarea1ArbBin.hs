data ArbBinario = L | N BinTree Bintree | N1 Bintree deriving (Eq, Show) 

-- Crea arbol binario completo de tamano 2(n+1)-1
crearArbBinario 0 = L 
crearArbBinario n = N (crearArbBinario(n-1))(crearArbBinario(n-1))

hacerABinTree s:: Integer -> Integer -> ArbBinario
hacerABinTree s n 
	| (2*n) + 1 <= s -> N (hacerABinTree s ((2*n)) (hacerABinTree s ((2*n) + 1))
	| (2*n) <=  s -> N1 (hacerABinTree((2*n))
	| otherwise L

-- Calcula el tamano del arbol 
tam L = 1
tam (N t1 t2) = 1 + tam t1 + tam t2
tamanom (N1 t3) = 1 + tam t3