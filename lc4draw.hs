module LC4Draw where

import System.Exit (exitSuccess, exitFailure)
import Graphics.Vty
import Control.Monad.State
import qualified Data.Map as M

import LC4VM
import Simulator

debuggerPrompt    = "(psdb) "
debuggerCmdString = unlines 
  [ "q (quit), h (help), b (breakpoint), c (continue), n (next), "
  , "set (PC, register, or memory), rs (reset), ld (load script)" ]

------------------------------------------------------------------------------
-- State needed for UI
------------------------------------------------------------------------------

-- Essentially a wrapper on VMState
data DebuggerState = DS { vm        :: VMState,
                          vty       :: Vty,
                          history   :: [String],
                          stdinDS   :: String,
                          stdoutDS  :: String }

------------------------------------------------------------------------------
-- Use Vty functions to draw UI elements
------------------------------------------------------------------------------

-- Some globals & reusable elements
defaultAttr, titleAttr :: Attr
defaultAttr = with_style current_attr default_style_mask
titleAttr   = with_style current_attr reverse_video

-- | Draw all the Instructions as lines in an image. Ensure that the currently
--   executing instruction (given by PC) is within the view and highlight it.
drawEditor :: VMState -> Image
drawEditor vm | (M.size $ prog vm) == 0 = empty_image
              | otherwise = string defaultAttr "TODO editor"

-- | Draw the box that represents a register
drawRegister :: (Register, Int) -> Image
drawRegister (_, _) = string defaultAttr "TODO register"

-- | Draw a horizontal list of register images
drawRegFile :: RegisterFile -> Image
drawRegFile = horiz_cat . (map drawRegister) . M.toList

-- | Draw the table entry that represents a memory location
drawEntry :: (Int, Int) -> Image
drawEntry (_, _) = string defaultAttr "TODO entry"

-- | Draw a vertical list of memory table entries
drawMemory :: Memory -> Image
drawMemory = vert_cat . (map drawEntry) . M.toList

-- | Image representing the debugger prompt
prompt :: Image
prompt = string defaultAttr debuggerPrompt

-- | Declaratively generate the Picture to be drawn by Vty, composed of Images
-- Should print the last command, current state, then the prompt
drawDebugger :: DebuggerState -> Picture
drawDebugger ds = pic_for_image debugger where
  vmState   = vm ds
  debugger  = title <-> (source <|> (registers <-> memImage)) <-> console

  source    = drawEditor (vm ds)
  registers = regFileTitle <-> (drawRegFile $ regFile vmState)
  memImage  = memoryTitle  <-> (drawMemory  $ memory vmState)
  console   = debuggerCmds <-> (prompt <|> input) <-> output

  input     = string defaultAttr (stdoutDS ds)
  output    = string defaultAttr (stdinDS ds)

  title         = string titleAttr "PennSim debugger for LC4 (psdb)"
  regFileTitle  = string titleAttr "Register File"
  memoryTitle   = string titleAttr "Memory"
  debuggerCmds  = string defaultAttr debuggerCmdString

------------------------------------------------------------------------------
-- Interact with the debugger console
------------------------------------------------------------------------------

-- | Write a string to the console
consoleLog :: String -> DebuggerState -> DebuggerState
consoleLog s ds = ds { stdoutDS = s }

-- | Append a character to stdin
appendToInput :: Char -> DebuggerState -> DebuggerState
appendToInput c ds = ds { stdinDS = (stdinDS ds) ++ [c] }

-- | Remove last character from stdin
backSpace :: DebuggerState -> DebuggerState
backSpace ds = ds { stdinDS = take (length input - 1) input }
  where input = stdinDS ds

-- | Flush stdin
clearInput :: DebuggerState -> DebuggerState
clearInput ds = ds { stdinDS = ""}

------------------------------------------------------------------------------
-- Event handling & command execution
------------------------------------------------------------------------------

-- | Execute the command the user typed in stdin
execInput :: DebuggerState -> IO DebuggerState
execInput ds
  | cmd `elem` ["q", "quit"] = exitDebugger ds'
  {-
  | cmd `elem` ["b", "breakpoint"] = setBreakLine s
  | cmd `elem` ["c", "continue"] = continueInstruction s
  | cmd `elem` ["n", "next"] = nextInstruction s
  | cmd `elem` ["bl"] = setBreakLabel s
  | cmd `elem` ["rs", "reset"] = resetVM s 
  | cmd `elem` ["sc", "script"] = setScript s >> return s
  -}
  | otherwise = return $ consoleLog "Command not recognized." ds'
  where
    cmd = stdinDS ds
    ds' = clearInput ds

-- | Respond to keyboard events
processEvent :: Key -> DebuggerState -> IO DebuggerState
processEvent key ds = case key of
  KEnter    -> execInput ds
  KDel      -> return $ backSpace ds
  KBackTab  -> return $ backSpace ds
  KASCII c  -> return $ appendToInput c ds
  otherwise -> return ds

------------------------------------------------------------------------------
-- Visual REPL for debugger
------------------------------------------------------------------------------

-- | Force Vty to wait for an accepted simulator event (keypress, enter)
waitForDebuggerCmd :: Vty -> IO Key
waitForDebuggerCmd vty = do
  event <- next_event vty
  case event of
    EvKey k _       -> case k of
                         KASCII _  -> return k
                         KEnter    -> return k
                         otherwise -> waitForDebuggerCmd vty
    EvMouse _ _ _ _ -> waitForDebuggerCmd vty
    EvResize _ _    -> waitForDebuggerCmd vty

repl :: DebuggerState -> IO ()
repl ds = do
  update (vty ds) $ drawDebugger ds
  key <- waitForDebuggerCmd $ vty ds
  ds' <- processEvent key ds
  repl ds'

-- | Generate an initial Debugger state
mkDebuggerState :: VMState -> Vty -> DebuggerState
mkDebuggerState vm vty =
  DS vm vty [] "" "Console output here"

-- | Create a Vty instance and use it to start the REPL
runDebugger :: VMState -> IO ()
runDebugger vm = do
  vty <- liftIO mkVty
  let ds = mkDebuggerState vm vty
  repl ds
  shutdown vty

-- | Safely exit the debugger program and its graphical context
exitDebugger :: DebuggerState -> IO DebuggerState
exitDebugger ds = do
  shutdown (vty ds)
  exitSuccess
