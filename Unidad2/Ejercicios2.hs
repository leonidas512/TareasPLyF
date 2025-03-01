module Ejercicios2 where
import Data.List (genericLength)

-- Aplicar descuentos e IVA

descuentos :: (Float, Float) -> Float
descuentos (precio, descuento) = precio - (precio * descuento / 100)

aplicariva :: (Float,Float) -> Float
aplicariva (precio,iva) = precio + (precio*iva/100)


--Aplicar una funcion a una lista

aplicarAListaRec :: (a -> b) -> [a] -> [b]
aplicarAListaRec _ [] = []
aplicarAListaRec f (x:xs) = f x : aplicarAListaRec f xs


--Diccionario de palabras
diccionario :: String -> [String]
diccionario = words

--Evaluando calificaicones
calificaciones :: [Int] -> [String]
calificaciones= map calificacion 
  where
    calificacion a
        | a >= 95 && a <= 100 = "excelente"
        | a < 95 && a >= 84   = "destacable"
        | a < 84 && a >= 75   = "bueno"
        | a < 75 && a >= 70   = "suficiente"
        | otherwise           = "desempeño insuficiente"

--Modulo de un vector

modulo :: (Float,Float) -> Float
modulo (largo, ancho) = sqrt( (largo*largo)+(ancho*ancho))


-- Función para calcular la media de una lista de números
media :: [Double] -> Double
media xs = sum xs / genericLength xs

-- Función para calcular la desviación estándar con n-1
desviacionEstandar :: [Double] -> Double
desviacionEstandar xs 
    | n < 2     = 0  -- Evita división por 0 si hay menos de 2 elementos
    | otherwise = sqrt (sum [(x - m) ^ 2 | x <- xs] / (n - 1))
  where
    m = media xs
    n = genericLength xs

-- Función para calcular la puntuación típica (z-score)
puntuacionTipica :: [Double] -> [Double]
puntuacionTipica xs 
    | d == 0    = replicate (length xs) 0  -- Evita división por 0
    | otherwise = [(x - m) / d | x <- xs]
  where
    m = media xs
    d = desviacionEstandar xs

-- Función para obtener los valores atípicos
valoresAtipicos :: [Double] -> [Double]
valoresAtipicos xs = [x | (x, z) <- zip xs (puntuacionTipica xs), abs z > 3]
