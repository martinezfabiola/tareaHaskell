{-|
Universidad Simón Bolívar
CI3661: Lenguajes de Programacion
Tarea 2

Autores:
- David Cabeza
- Fabiola Martínez 

Problema 3: Monads

-}

newtype Secuencial s a = Secuencial (s -> (a, s))

-- Pregunta a)

-- Secuencial recibe dos argumentos, s y a. Por otra parte Monad espera un
-- constructor el cual debe tener un argumento y como los constructores son
-- funciones, despues que se pasa el primer parametro se obtiene un nuevo
-- constructor que solo recibe un paramentro, s. 


-- Pregunta b)

-- return :: a -> Secuencial s a
-- (>>=) :: Secuencial s a ->  (a -> Secuencial s b) -> Secuencial s b
-- (>>) :: Secuencial s a -> Secuencial s b -> Secuencial s b 
-- fail :: String -> Secuencial s a


-- Pregunta c)

return a = \s -> (a,s)

-- Pregunta d)

(Secuencial programa) >> transformador =
      Secuencial $ \estadoInicial ->
            let (resultado, nuevoEstado) = programa estadoInicial
                (Secuencial nuevoPrograma)  = transformador resultado
            in nuevoPrograma nuevoEstado