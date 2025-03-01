module Cabal where

import Codec.Picture
import Codec.Picture.Types
import Data.Array.Repa as R
import qualified Data.Vector.Storable as V

-- Convertir una imagen de JuicyPixels a una matriz de Repa
juicyToRepa :: Image Pixel8 -> Array U DIM2 Double
juicyToRepa img = fromListUnboxed (Z :. h :. w) (fmap (fromIntegral . V.unsafeIndex (imageData img)) [0..(w*h-1)])
  where
    (w, h) = (imageWidth img, imageHeight img)

-- Convertir una matriz de Repa a una imagen de JuicyPixels
repaToJuicy :: Array U DIM2 Double -> Image Pixel8
repaToJuicy arr = generateImage (\x y -> fromIntegral (round (arr R.! (Z :. y :. x)))) w h
  where
    (Z :. h :. w) = extent arr

-- Verificar si una coordenada está dentro del rango válido
inRange :: (Int, Int) -> Int -> Bool
inRange (low, high) x = x >= low && x <= high

-- Aplicar un filtro de desenfoque
blur :: Array U DIM2 Double -> Array U DIM2 Double
blur arr = computeS $ R.fromFunction (extent arr) avg
  where
    avg (Z :. y :. x) =
      let neighbors = [(dy, dx) | dy <- [-1..1], dx <- [-1..1]]
          valid (dy, dx) = inRange (0, h-1) (y + dy) && inRange (0, w-1) (x + dx)
          values = [arr R.! (Z :. (y + dy) :. (x + dx)) | (dy, dx) <- neighbors, valid (dy, dx)]
      in sum values / fromIntegral (length values)
    (Z :. h :. w) = extent arr

-- Convertir una imagen en escala de grises a binaria
binarize :: Array U DIM2 Double -> Array U DIM2 Double
binarize arr = computeS $ R.map (\x -> if x > 128 then 255 else 0) arr

main :: IO ()
main = do
  -- Cargar una imagen desde un archivo
  eitherImg <- readImage "../imgs/imagen.jpg"
  case eitherImg of
    Left err -> putStrLn err
    Right dynImg -> do
      let img = convertRGB8 dynImg  -- Convertir la imagen a RGB8
      let grayImg = pixelMap rgb8ToPixel8 img  -- Convertir a escala de grises
      let repaImg = juicyToRepa grayImg  -- Convertir a matriz de Repa
      let blurredImg = blur repaImg  -- Aplicar el filtro de desenfoque
      let binaryImg = binarize blurredImg  -- Convertir a imagen binaria
      let processedImg = repaToJuicy binaryImg  -- Convertir de vuelta a imagen de JuicyPixels
      savePngImage "../imgs/imagen-procesada.png" (ImageY8 processedImg)  -- Guardar la imagen procesada

-- Función auxiliar para convertir un pixel RGB8 a escala de grises
rgb8ToPixel8 :: PixelRGB8 -> Pixel8
rgb8ToPixel8 (PixelRGB8 r g b) = round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)
