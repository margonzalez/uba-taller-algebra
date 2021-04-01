-- Implementar las siguientes funciones, especificando su signatura. Usar pares para
-- representar vectores. ******Opcional: chequear qué tipo asigna Haskell cuando se elimina la
-- signatura y argumentar las razones de cada tipo.*******
-- 1
-- estanRelacionados: dados dos números reales, decide si están relacionados considerando
-- la relación de equivalencia en R cuyas clases de equivalencia son:
-- (−∞, 3], (3, 7] y (7, ∞).
-- 2 prodInt: calcula el producto interno entre dos vectores de R 2 .
-- 3 todoMenor: dados dos vectores de R 2 , decide si es cierto que cada coordenada del primer
-- vector es menor a la coordenada correspondiente del segundo vector.
-- 4 distanciaPuntos: calcula la distancia entre dos puntos de R 2 .
-- 5 sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
-- 6 posicPrimerPar: dada una terna de enteros, devuelve la posición del primer número par si
-- es que hay alguno, y devuelve 4 si son todos impares.
-- 7 crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por
-- separado (debe funcionar para elementos de cualquier tipo).
-- 8 invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como parámetro
-- (debe funcionar para elementos de cualquier tipo).

module Clase02
where
    import Clase01

    menorOIgualA3 :: Float -> Bool
    menorOIgualA3 x = x <= 3

    mayorA7 :: Float -> Bool
    mayorA7 x = x > 7

    entre3y7 :: Float -> Bool
    entre3y7 x = x > 3 && x <= 7

    estanRelacionados :: Float -> Float -> Bool
    estanRelacionados x y | menorOIgualA3 x && menorOIgualA3 y = True
                          | entre3y7 x && entre3y7 y = True
                          | mayorA7 x && mayorA7 y = True
                          | otherwise = False


    prodInt :: (Float, Float) -> (Float, Float) -> Float
    prodInt v1 v2 = fst v1 * fst v2 + snd v1 * snd v2


    todoMenor :: (Float, Float) -> (Float, Float) -> Bool
    todoMenor (a, b) (c, d) = a < c && b < d


    distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
    distanciaPuntos (x1, y1) (x2, y2) = sqrt ((x2 - x1)**2 + (y2 -y1)**2)


    sumaTerna :: (Int, Int, Int) -> Int
    sumaTerna (x, y, z) = x + y + z


    posicPrimerPar :: (Int, Int, Int) -> Int
    posicPrimerPar (x, y, z) | even x = 1
                            | even y = 2
                            | even z = 3
                            | otherwise = 4


    crearPar :: t1 -> t2 -> (t1, t2)
    crearPar a b = (a, b)


    invertir :: (t1, t2) -> (t2, t1)
    invertir (a, b) = (b, a)
