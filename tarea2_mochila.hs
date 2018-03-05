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

-- dado un item devuelve la tupla con su peso
compararAux2 :: Eq a => Pesos a -> a -> (a, Integer)
compararAux2 listaValores item = head (filter (\(x,_) -> x == item) listaValores)

-- dada una lista de item devueve una lista de items con su respectivo peso
obtenerPesos :: Eq a => [[a]] -> Pesos a -> [Pesos a]
obtenerPesos items valorItems1 = map comparar items
 where comparar x = map (compararAux2 valorItems1) x

-- devuelve una lista con los posibles pesos candidatos
aplicarFiltrarMaxPesos :: Eq a => [a] -> Integer -> Pesos a -> [Pesos a]
aplicarFiltrarMaxPesos items capacidadMochila listaPesos = filtrarMaxPesos capacidadMochila (obtenerPesos (posiblesItems items) listaPesos)

-- dado un item devuelve la tupla con su valor
compararAux :: Eq a => Valor a -> a -> (a, Integer)
compararAux listaValores item = head (filter (\(x,_) -> x == item) listaValores)

-- dada una lista de item devueve una lista de items con su respectivo valor
obtenerValores :: Eq a => [[a]] -> Valor a -> [Valor a]
obtenerValores items valorItems1 = map comparar items
 where comparar x = map (compararAux valorItems1) x

-- dada una lista de items con valor devuelve una lista cuya primera coordenada 
-- es un lista de item y la segunda la suma de sus valores
obtenerMaxValor :: [Valor a] -> [([a],Integer)]
obtenerMaxValor listaValores = map suma (map unzip listaValores)
 where 
    suma x = (fst x, sum(snd x))

-- dada una lista de items con la suma de sus valores devuelve la lista de items 
-- que tenga maximo valor       
filtrarMaxVal :: [([a],Integer)] -> [[a]]
filtrarMaxVal candidatos = map fst (filter (\(_,x) -> x == maximum (map snd candidatos)) candidatos)

-- aplica todas las funciones anteriores para resolver el problema de la mochila
problemaMochila :: Eq a => [a] -> Integer -> Pesos a -> Valor a -> [[a]]
problemaMochila items capacidadMochila listaPesos listaValores = filtrarMaxVal (obtenerMaxValor (obtenerValores (obtenerListaItem(aplicarFiltrarMaxPesos items capacidadMochila listaPesos) listaValores)))


