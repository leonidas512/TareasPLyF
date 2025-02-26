module Evaluacion1 where

-- Función para verificar si un número es primo
esPrimo :: Int -> Bool
esPrimo n
    | n < 2     = False      
    | n == 2    = True        
    | even n    = False     
    | otherwise = null [x | x <- [3, 5..isqrt n], n `mod` x == 0] 
  where
    isqrt = floor . sqrt . fromIntegral

-- Función para números menores a 20
menoresA20 :: Int -> String
menoresA20 n
    | n == 0    = "Cero"
    | n == 1    = "Uno"
    | n == 2    = "Dos"
    | n == 3    = "Tres"
    | n == 4    = "Cuatro"
    | n == 5    = "Cinco"
    | n == 6    = "Seis"
    | n == 7    = "Siete"
    | n == 8    = "Ocho"
    | n == 9    = "Nueve"
    | n == 10   = "Diez"
    | n == 11   = "Once"
    | n == 12   = "Doce"
    | n == 13   = "Trece"
    | n == 14   = "Catorce"
    | n == 15   = "Quince"
    | n == 16   = "Dieciséis"
    | n == 17   = "Diecisiete"
    | n == 18   = "Dieciocho"
    | n == 19   = "Diecinueve"
    | otherwise = ""

-- Función para las decenas
decenas :: Int -> String
decenas n
    | n < 20    = menoresA20 n
    | n == 20   = "Veinte"
    | n == 30   = "Treinta"
    | n == 40   = "Cuarenta"
    | n == 50   = "Cincuenta"
    | n == 60   = "Sesenta"
    | n == 70   = "Setenta"
    | n == 80   = "Ochenta"
    | n == 90   = "Noventa"
    | otherwise = decenas (n `div` 10 * 10) ++ " y " ++ menoresA20 (n `mod` 10)

-- Función para las centenas
centenas :: Int -> String
centenas n
    | n== 0 = decenas (n `mod` 100)
    | n == 100  = "Cien"
    | n < 200   = "Ciento " ++ decenas (n `mod` 100)
    | n == 200  = "Doscientos"
    | n == 300  = "Trescientos"
    | n == 400  = "Cuatrocientos"
    | n == 500  = "Quinientos"
    | n == 600  = "Seiscientos"
    | n == 700  = "Setecientos"
    | n == 800  = "Ochocientos"
    | n == 900  = "Novecientos"
    | otherwise = centenas (n `div` 100 * 100) ++ " " ++ decenas (n `mod` 100)

-- Función para los miles
miles :: Int -> String
miles n
    | n < 1000  = centenas n
    | n == 1000 = "Mil"
    | n < 2000  = "Mil " ++ centenas (n `mod` 1000)
    | otherwise = centenas (n `div` 1000) ++ " mil " ++ centenas (n `mod` 1000)

-- Función para los millones
millones :: Int -> String
millones n
    | n == 1000000 = "Un millón"
    | otherwise = miles n

-- Función principal para convertir un número a palabras
numero :: Int -> String
numero n
    | n < 20    = menoresA20 n
    | n < 100   = decenas n
    | n < 1000  = centenas n
    | n < 1000000 = miles n
    | n == 1000000 = millones n
    | otherwise = "Número fuera de rango"

-- Función FizzBuzz
fizzbuzz :: Int -> String
fizzbuzz n
    | esPrimo n = "FizzBuzz!"
    | otherwise = numero n


main :: IO ()
main = do
    print $ fizzbuzz 7
    print $ fizzbuzz 785164
    print $ fizzbuzz 843125 
    print $ fizzbuzz 156349
    print $ fizzbuzz 131546