module LC4Draw where

import System.Exit (exitSuccess)
import Graphics.Vty
import Control.Monad.State
import qualified Data.Map as M
import Numeric
import Data.Word (Word)

import LC4VM
import LC4PP
import Simulator

debuggerPrompt :: String
debuggerPrompt = "(psdb) "

debuggerCmds :: [String]
debuggerCmds = [ "q (quit), h (help), b (breakpoint), c (continue), n (next), \
                 \set (PC, register, or memory), ", 
                 "rs (reset), ld (load script)" ]

welcomeMsg :: String
welcomeMsg =
  "Welcome to the LC4 Simulator. This is a general purpose \
   \interpreter for the LC4 assembly language. Type h or help for help.\n"

-- | The help text that is displayed for help.
helpText :: String
helpText =
  " This is a list of commands available to you. Some commands expect \
  \an argument. \n \
  \\n \
  \q  | quit     -- Leave the interpreter.\n \
  \h  | help     -- Print this message. \n \
  \\n \
  \n  | next     -- Step to the next instruction.\n \
  \c  | continue -- Continue to breakpoint/end of program. \n \
  \\n \
  \r  | reg      -- Show the register file. \n \
  \pc |          -- Show the program file. \n \
  \m  | mem      -- Show current memory. \n \
  \l  | lbl      -- Show labels. \n \
  \bp |          -- Show current breakpoints. \n \
  \p  |          -- Show program. \n \
  \b  |          -- Set a line breakpoint. \n \
  \bl |          -- Set a label breakpoint. \n \
  \rs |          -- Reset. \n \
  \sc |          -- Load script. \n"

------------------------------------------------------------------------------
-- State needed for Debugger UI
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
defaultAttr, inverseAttr, titleAttr, redAttr, highlight :: Attr
defaultAttr = ((def_attr `with_style` default_style_mask)
               `with_fore_color` white) `with_back_color` black
inverseAttr = current_attr `with_style` reverse_video
titleAttr   = current_attr `with_fore_color` bright_red
redAttr     = defaultAttr `with_fore_color` bright_cyan
highlight   = defaultAttr `with_fore_color` bright_yellow

-- | Draw all the Instructions as lines in an image. Ensure that the currently
--   executing instruction (given by PC) is within the view and highlight it.
drawEditor :: VMState -> Image
drawEditor vms | (M.size $ prog vms) == 0 = empty_image
               | otherwise = vert_cat $ map (drawLine (pc vms))
                                            (M.toList $ prog vms)
  where
    drawLine :: PC -> (Int, Instruction) -> Image
    drawLine pc (curPC, insn) = (drawLineNr curPC) <|>
                                (string attr $ display insn)
      where attr = if pc == curPC then highlight
                                  else defaultAttr
    drawLineNr :: Int -> Image
    drawLineNr i = pad (fromIntegral maxWidth, 1) $ string inverseAttr (show i)

    -- Find the length of the longest line number in the program
    maxWidth :: Int
    maxWidth = 1 + (length $ show $ fst $ M.findMax (prog vms))

-- | Draw a string representing an int in base 16
drawHex :: Attr -> Int -> Image
drawHex attr val = pad (8, 1) $ string attr ('x' : showHex val "")

-- | Draw a horizontal list of register images
drawRegFile :: RegisterFile -> Image
drawRegFile rf = (filterRegisters (\r _ -> r `elem` [R0, R1, R2, R3])) <->
                 (filterRegisters (\r _ -> r `elem` [R4, R5, R6, R7]))
  where 
  drawRow rs = (pad (1, 2)) $ horiz_cat $ map drawRegister rs
  filterRegisters pred = drawRow $ M.toList $ M.filterWithKey pred rf

  drawRegister :: (Register, Int) -> Image
  drawRegister (reg, val) = (pad (4, 1) $ string redAttr (show reg)) <|>
                            (drawHex defaultAttr val)

-- | Draw a vertical list of memory table entries
drawMemory :: Memory -> Image
drawMemory = vert_cat . (map drawEntry) . M.toList where
  -- Draw the table entry that represents a memory location
  drawEntry :: (Int, Int) -> Image
  drawEntry (loc, val) = drawHex redAttr loc <|> drawHex defaultAttr val <|>
                         (pad (10, 1) $ string defaultAttr (show val))

-- | Declaratively generate the Picture to be drawn by Vty, composed of Images
-- Should print the last command, current state, then the prompt
drawDebugger :: DebuggerState -> Picture
drawDebugger ds = pic_for_image debugger where
  vmState   = vm ds
  debugger  = title <-> (source <|> (registers <-> memImage)) <-> console

  source    = pad (40, 40) $ drawEditor (vm ds)
  registers = regFileTitle <-> (drawRegFile $ regFile vmState)
  memImage  = memoryTitle  <-> (drawMemory  $ memory vmState)
  console   = {-pad (100, 10) $ -} debuggerHelp <-> input <-> output

  output        = string (defaultAttr `with_fore_color` green) (stdoutDS ds)
  input         = string ((defaultAttr `with_fore_color` bright_green) 
                          `with_style` underline)
                         (debuggerPrompt ++ stdinDS ds)
  title         = string inverseAttr "PennSim debugger for LC4" <|>
                  string defaultAttr ""
  regFileTitle  = string (titleAttr `with_style` reverse_video) "Register File"
  memoryTitle   = string (titleAttr `with_style` reverse_video) "Memory"
  debuggerHelp  = pad (50, 4) $ vert_cat $ map (string inverseAttr) debuggerCmds

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
backSpace ds = ds { stdinDS = take (length input - 2) input }
  where input = stdinDS ds

-- | Flush stdin
clearInput :: DebuggerState -> DebuggerState
clearInput ds = ds { stdinDS = ""}

------------------------------------------------------------------------------
-- Event handling & command execution
------------------------------------------------------------------------------

-- | Wrapper for `nextStep`
stepDebugger :: DebuggerState -> DebuggerState
stepDebugger ds = ds { vm = execState nextStep (vm ds) }

-- | Wrapper for `continue`
continueDebugger :: DebuggerState -> DebuggerState
continueDebugger ds = ds { vm = execState continue (vm ds) }

-- | Execute the command the user typed in stdin
execInput :: DebuggerState -> IO DebuggerState
execInput ds
  | cmd `elem` ["q", "quit"] = exitDebugger ds'
  | cmd `elem` ["h", "help"] = return $ consoleLog helpText ds'
  | cmd `elem` ["n", "next"] = return $ stepDebugger ds'
  | cmd `elem` ["c", "continue"] = return $ continueDebugger ds'
  {-
  | cmd `elem` ["b", "breakpoint"] = setBreakLine s
  | cmd `elem` ["bl"] = setBreakLabel s
  | cmd `elem` ["rs", "reset"] = resetVM s 
  | cmd `elem` ["sc", "script"] = setScript s >> return s
  -}
  | otherwise = return $ consoleLog "Command not recognized." ds'
  where
    cmd = stdinDS ds    -- TODO: strip whitespace
    ds' = clearInput ds

-- | Respond to keyboard events
processEvent :: Key -> DebuggerState -> IO DebuggerState
processEvent key ds = case key of
  KEnter    -> execInput ds
  KDel      -> return $ backSpace ds
  KBackTab  -> return $ backSpace ds
  KASCII c  -> return $ appendToInput c ds
  _         -> return ds

------------------------------------------------------------------------------
-- Visual REPL for debugger
------------------------------------------------------------------------------

-- | Force Vty to wait for a keypress
waitForKeypress :: Vty -> IO Key
waitForKeypress vty' = do
  event <- next_event vty'
  case event of
    EvKey k _       -> return k
    EvMouse _ _ _ _ -> waitForKeypress vty'
    EvResize _ _    -> waitForKeypress vty'

repl :: DebuggerState -> IO ()
repl ds = do
  update (vty ds) $ drawDebugger ds
  key <- waitForKeypress $ vty ds
  ds' <- processEvent key ds
  repl ds'

-- | Generate an initial Debugger state
mkDebuggerState :: VMState -> Vty -> DebuggerState
mkDebuggerState vms vty' =
  DS vms vty' [] "" ""

-- | Create a Vty instance and use it to start the REPL
runDebugger :: VMState -> IO ()
runDebugger vms = do
  vty' <- liftIO mkVty
  let ds = mkDebuggerState vms vty'
  repl ds
  shutdown vty'

-- | Safely exit the debugger program and its graphical context
exitDebugger :: DebuggerState -> IO DebuggerState
exitDebugger ds = do
  shutdown (vty ds)
  exitSuccess
