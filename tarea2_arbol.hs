{-|
Universidad Simón Bolívar
CI3661: Lenguajes de Programacion
Tarea 2

Autores:
- David Cabeza
- Fabiola Martínez 

Problema 1: Arbol Binario

-}

import Data.List

-- definimos arbol binario
data Arbol a = Hoja a| Nodo a (Arbol a) (Arbol a) deriving (Show, Eq) 

-- coloca los nodos de un arbol en una lista
hacerLista :: Arbol a -> [a]
hacerLista (Hoja r) = [r]
hacerLista (Nodo r nodoIzq nodoDer) = r : (hacerLista nodoIzq ++ hacerLista nodoDer)

-- compara si los nodos de un arbol estan contenidos dentro del otro
contenidos :: Eq a => Arbol a -> Arbol a -> Bool
contenidos arbol1 arbol2 = isInfixOf (hacerLista arbol1) (hacerLista arbol2)
