module Ejercicios where

-- Suma de una lista de enteros
sumatoria :: [Int] -> Int
sumatoria = sum

-- Factorial de un número
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Generar una lista de números pares hasta n
pares :: Int -> [Int]
pares n = [x | x <- [1..n], even x]

-- Longitud de una cadena
longitud :: String -> Int
longitud = length

-- Invertir una lista
reves :: [Int] -> [Int]
reves [] = []
reves (x:xs) = reves xs ++ [x]

-- Duplicar cada elemento de una lista
duplicar :: [Int] -> [Int]
duplicar = map (*2)

-- Filtrar los números pares de una lista
filtrarPares :: [Int] -> [Int]
filtrarPares = filter even

-- Fibonacci de un número n
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Divisores de un número
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- Comprobar si una palabra es un palíndromo
palindromo :: String -> Bool
palindromo p = p == reverse p
