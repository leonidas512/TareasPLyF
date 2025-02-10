module FizzBuzz where

-- enlista los numeros de 0 a 19 para poder obtenerlos con letras al no ser multiplos de 3 o 5
menoresA20 :: [String]
menoresA20 = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
              "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

-- enlista los nombres de las decenas para los numeros compuestos
decenas :: [String]
decenas = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]


-- separa las decenas de los numeros de 1 a 9 para poder usarlos
letras :: Int -> String
letras n
    | n < 20    = menoresA20 !! n
    | n < 100   = let (tens, units) = n `divMod` 10
                  in if units == 0
                     then decenas !! tens
                     else decenas !! tens ++ " " ++ menoresA20 !! units
    | otherwise = "Número fuera del rango"

-- hace la comparativa para las comparativas de fizzbuzz
fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 3 == 0 && n `mod` 5 == 0 = "FizzBuzz"
    | n `mod` 3 == 0                   = "Buzz"
    | n `mod` 5 == 0                   = "Fizz"
    | otherwise                        = letras n

-- permite hacer pruebas desde la ejecucion de visual studio code
main :: IO ()
main = do
    print $ fizzbuzz 16  
    print $ fizzbuzz 30  
    print $ fizzbuzz 25  
    print $ fizzbuzz 9   
    print $ fizzbuzz 42
