-- Implementar las siguientes funciones, especificando su signatura.

-- 1 absoluto: calcula el valor absoluto de un n´umero entero.
-- 2 maximoabsoluto: devuelve el m´aximo entre el valor absoluto de dos n´umeros enteros.
-- 3 maximo3: devuelve el m´aximo entre tres n´umeros enteros.
-- 4 algunoEs0: dados dos n´umeros racionales, decide si alguno de los dos es igual a 0 (hacerlo
-- dos veces, una sin usar y otra usando pattern matching).
-- 5 ambosSon0: dados dos n´umeros racionales, decide si ambos son iguales a 0 (hacerlo dos
-- veces, una sin usar y otra usando pattern matching).
-- 6 esMultiploDe: dados dos n´umeros naturales, decidir si el primero es m´ultiplo del segundo.
-- 7 digitoUnidades: dado un n´umero natural, extrae su d´ıgito de las unidades.
-- 8 digitoDecenas: dado un n´umero natural, extrae su d´ıgito de las decenas.

-- Observacion: Cuando el problema en cuesti´on trata sobre n´umeros naturales, se puede
-- simplemente usar el tipo Int e ignorar el comportamiento del programa si el usuario
-- decide ejecutarlo usando para los par´ametros enteros negativos o 0.


module Clase01
where
    absoluto :: Int -> Int
    absoluto n  | n > 0 = n
                | otherwise = -n

    maximoabsoluto :: Int -> Int -> Int
    maximoabsoluto x y = max (absoluto x) (absoluto y)


    maximo3 :: Int -> Int -> Int -> Int
    maximo3 x y z = maximum [x, y, z]

    algunoEs0 :: Float -> Float -> Bool
    algunoEs0 _ 0 = True
    algunoEs0 0 _ = True
    algunoEs0 _ _ = False


    es0 :: Float -> Bool
    es0 n = n == 0

    algunoEs0v2 :: Float -> Float -> Bool 
    algunoEs0v2 x y = es0 x || es0 y


    ambosSon0 :: Float -> Float -> Bool 
    ambosSon0 0 0 = True
    ambosSon0 _ _ = False


    ambosSon0v2 :: Float -> Float -> Bool 
    ambosSon0v2 x y = es0 x && es0 y


    esMultiploDe :: Int -> Int -> Bool 
    esMultiploDe x y = mod x y == 0


    digitoUnidades :: Int -> Int
    digitoUnidades x = mod x 10


    digitoDecenas :: Int -> Int
    digitoDecenas x | x < 0 = undefined
                    | x <= 9 = digitoUnidades x
                    | otherwise = div (mod x 100) 10

