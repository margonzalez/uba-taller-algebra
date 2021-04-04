-- 1 Escribir una funci´on para determinar si un n´umero natural es m´ultiplo de 3. No est´a
-- permitido utilizar mod ni div.
-- 2 Implementar la funci´on sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n
-- n´umeros impares. Ej: sumaImpares 3 1+3+5 9.
-- 3 Escribir una funci´on medioFact que dado n ∈ N calcula n!! = n (n − 2)(n − 4) · · · . Por
-- ejemplo:
-- medioFact 10 10 ∗ 8 ∗ 6 ∗ 4 ∗ 2 3840.
-- medioFact 9 9 ∗ 7 ∗ 5 ∗ 3 ∗ 1 945.
-- 4 Escribir una funci´on que determine la suma de d´ıgitos de un n´umero positivo. Para esta
-- funci´on pueden utilizar div y mod.
-- 5 Implementar una funci´on que determine si todos los d´ıgitos de un n´umero
-- son iguales

module Clase03
where
    import Clase02

    esMultiploDe3SinPreludio :: Int -> Bool
    esMultiploDe3SinPreludio x | x == 0 = True
                               | x < 3 = False
                               | otherwise = esMultiploDe3SinPreludio (x - 3)

    sumaImpares :: Int -> Int
    sumaImpares 0 = 0
    sumaImpares x = ((x * 2) - 1) + sumaImpares (x - 1)


    factorial n | n == 0 = 1
                | n > 0 = n * factorial (n-1)

    medioFact :: Int -> Int
    medioFact n | n == 0 = 1
                | n == 1 = 1
                | n > 0 = n * medioFact(n - 2)

    sumaDigitosPositivos :: Int -> Int
    sumaDigitosPositivos n | n < 0 = undefined
                           | n == 0 = 0
                           | otherwise = n + sumaDigitosPositivos (n - 1)


    digitosIguales :: Int -> Int -> Bool
    digitosIguales x y = x == y

    todosDigitosIguales :: Int -> Bool
    todosDigitosIguales n | div n 10 == 0 = True
                          | otherwise = digitosIguales (mod n 10) (mod (div n 10) 10) && todosDigitosIguales (div n 100)
