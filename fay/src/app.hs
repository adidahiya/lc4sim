{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Demo code for Fay-to-JS compilation

import Language.Fay.Prelude
import Language.Fay.FFI
import JQuery
import Canvas

-- import Control.Monad.State
-- import Data.Map
-- import Data.Set
import State
import Bits
import LC4VM

map = Language.Fay.Prelude.map

------------------------------------------------------------------------------
-- Interact with LC4 Simulator and update UI
------------------------------------------------------------------------------

-- RegisterFile :: Map Register Int
printRegisters :: VMState -> Fay ()
printRegisters vm = Language.Fay.Prelude.sequence_ (Main.map printReg [0..7]) where

  printReg :: Int -> Fay JQuery
  printReg r = do
    let reg = read ('R' : show r) :: Register
    regEl <- selectID ('R' : show r)
    -- setText ('x' : showHex val "") regEl
    let val = Data.Map.findWithDefault (error "missing reg") reg (regFile vm)
    setText (show val) regEl

-- Memory :: Map Int Int
printMem :: VMState -> Fay ()
printMem vm = Language.Fay.Prelude.sequence_ (Main.map printEntry [0..(mSize - 1)]) where
  mSize = (Data.Map.size mem)
  mem   = memory vm

  printEntry :: Int -> Fay JQuery
  printEntry e = do
    entries <- select "#memory .value"
    entryEl <- selectInstance e entries
    let val = Data.Map.findWithDefault (error "missing entry") e mem
    -- setText ('x' : showHex val "") entryEl
    setText (show val) entryEl

-- xC000
videoMemStart :: Int
videoMemStart = 49152

-- xFDFF
videoMemEnd :: Int
videoMemEnd = 65023

-- TODO: convert hex value (xF6000) to rgb value (50, 10, 0)
hexToRGBA :: Int -> RGBA
hexToRGBA = undefined

getVideoMem :: Memory -> Memory
getVideoMem = Data.Map.filter (\v -> v > videoMemStart && v < videoMemEnd)

-- Video memory resides from xC000 <-> xFDFF
drawVideoMem :: VMState -> Fay ()
drawVideoMem vm = Language.Fay.Prelude.sequence_ (Main.map drawAction mem) where
  canvas = ffi "document.getElementById('canvas')" -- TODO: fix

  mem :: [(Int, Int)]
  mem = Data.Map.toList $ getVideoMem $ memory vm

  drawAction :: (Int, Int) -> Fay ()
  drawAction (mem, hex) = drawPixel (hexToRGBA hex) (x, y) canvas where
    x = pixelWidth  * ((mem - videoMemStart) `mod` 128)
    y = pixelHeight * ((mem - videoMemStart) `div` 128)

------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Set up Ace Editor and its theme
initEditor :: Fay ()
initEditor = ffi "ace.edit('editor').setTheme('ace/theme/tomorrow')"

onLineClick :: Event -> Fay ()
onLineClick e = do
  putStrLn "got line click"
  print e

sampleVM :: VMState
sampleVM = VM { prog    = Data.Map.empty,
                lbls    = Data.Map.empty,
                regFile = undefined,
                memory  = Data.Map.empty,
                brks    = Data.Set.empty,
                pc      = 0,
                cc      = CC_N,
                psr     = False }

app :: Event -> Fay ()
app _ = do
  putStrLn "Start js app..."
  -- initEditor
  -- initFilepicker
  printRegisters sampleVM
  printMem sampleVM
  lineGutter <- selectClass "ace_gutter-cell"
  click onLineClick lineGutter
  putStrLn "...finished main."

theDocument :: Document
theDocument = ffi "window.document"

main :: Fay ()
main = documentReady app theDocument

