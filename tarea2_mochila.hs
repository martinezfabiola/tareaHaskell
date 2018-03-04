{-|
Universidad Simón Bolívar
CI3661: Lenguajes de Programacion
Tarea 2

Autores:
- David Cabeza
- Fabiola Martínez 

Problema 2: La mochila

-}

import Data.List

type Pesos a = [(a, Integer)]
type Valor a = [(a, Integer)]

-- obtenemos todos los subconjuntos posibles
posiblesItems :: [a] -> [[a]]
posiblesItems []  = [[]]
posiblesItems (x:xs) = posiblesItems xs ++ map (x:) (posiblesItems xs)

-- filtra por pesos menores a iguales a la capacidad de la mochila
filtrarMaxPesos :: Integer -> [Pesos a] -> [Pesos a]
filtrarMaxPesos capacidadMochila listaPesos  = filter aux listaPesos
 where
    aux x = sum(map snd x) <= capacidadMochila

-- devuelve una lista generada a partir de las primera coordenadas de otra lista
obtenerListaItem :: [[(a, Integer)]] -> [[a]]
obtenerListaItem lista = map fst (map unzip lista)

-- filtra la lista de valores a partir de la lista de pesos 
prueba :: Eq a => [Pesos a] -> [Valor a] -> [[a]]
prueba listaPesos listaValores = filter aux2 n
 where
    n = obtenerListaItem listaValores
    m = obtenerListaItem listaPesos
    aux2 elem = isInfixOf [elem] m

    

