-- This content is released under the (Link Goes Here) MIT License.

module LC4Draw where

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper
-- import UI.NCurses

import LC4VM

consoleREPL :: VMState -> IO ()
consoleREPL s = return ()

{-
drawMain :: CM ()
drawMain = do
  w <- mkMainWidget
  redraw w
  eventLoop w
-}

{-

consoleREPL :: VMState -> IO ()
consoleREPL s = runCurses $ do
  setEcho False
  w <- defaultWindow
  updateWindow w $ do
    moveCursor 1 10
    drawString "Hello World!"
    moveCursor 3 10
    drawString "(press q to quit)"
    moveCursor 0 0
  render
  waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop
      Just ev' -> if p ev' then return () else loop
-}

