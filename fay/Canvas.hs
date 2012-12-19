-- An interface for HTML5 Canvas functions

{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Canvas where

import Language.Fay.Prelude
import Language.Fay.FFI
import JQuery

data Canvas
instance Foreign Canvas

data RGBA = RGBA  { r :: Int, g :: Int, b :: Int, a :: Double }
  deriving (Eq)

pixelWidth, pixelHeight :: Int
pixelWidth  = 2
pixelHeight = 2

-- | Check if RGBA values are within correct ranges
valid :: RGBA -> Bool
valid val = r val > 255 || r val < 0 ||
            g val > 255 || g val < 0 ||
            b val > 255 || b val < 0 ||
            a val > 1.0 || a val < 0

printRGBA :: RGBA -> String
printRGBA val | valid val = error "Invalid rgba value"
              | otherwise = "rgba(" ++ show (r val) ++ ", " ++ show (g val) ++
                            ", " ++ show (b val) ++ ", " ++ show (a val) ++ ")"

-- | Given a DOM element, get the canvas context and execute some draw function
drawInCanvas :: Element -> (Canvas -> Fay ()) -> Fay ()
drawInCanvas el draw = do
  canvas <- ffi "%1.getContext('2d')" el
  draw canvas

-- | Set the fill style on a canvas element for subsequent drawing
setFillStyle :: RGBA -> Canvas -> Fay ()
setFillStyle val = ffi "%2.fillStyle = '%1'" (printRGBA val)

-- | Draw a pixel to the canvas at the specified x, y coordinates
drawPixel :: RGBA -> (Int, Int) -> Canvas -> Fay ()
drawPixel p (x, y) c = do
  setFillStyle p c
  ffi "%5.fillRect(%1, %2, %3, %4)" x y pixelWidth pixelHeight c

-- | Draw a 2D array of pixels to the canvas element.
--   Assumes that the element is large enough to hold the number of pixels
--   provided to the function.
drawPixels :: [[RGBA]] -> Canvas -> Fay ()
drawPixels _ _ = undefined

