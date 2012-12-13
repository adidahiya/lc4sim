-- This content is released under the (Link Goes Here) MIT License.
-- | LC4sim's REPL
-- Main is here

module Main where

import System.Exit (exitSuccess, exitFailure)
import System.IO
import System.Environment (getArgs)
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import qualified Data.Set as Set

import LC4Parser
import LC4VM
import ParserCombinators
import VMLoader
import Simulator
import LC4PP

-- | The welcome message. This is printed when the interpreter is started
--   without any command-line arguments.
welcomeMsg :: String
welcomeMsg =
  "Welcome to the LC4 Simulator. This is a general purpose \
   \interpreter for the LC4 assembly language. Type h or help for help.\n"

-- | The prompt displayed.
prompt :: String
prompt = "lc4sim> "

-- | The help text that is displayed for help.
helpText :: String
helpText =
  "This is a list of commands available to you. Some commands expect \
  \an argument. \n \
  \\n \
  \q  | quit     -- Leave the interpreter.\n \
  \h  | help     -- Print this message. \n \
  \c  | continue -- Continue to breakpoint/end of program. \n \
  \n  | next     -- Step to the next instruction.\n \
  \r  | reg      -- Show the register file. \n \
  \p  | prog     -- Show the program file. \n \
  \m  | mem      -- Show current memory. \n \
  \l  | lbl      -- Show labels. \n \
  \b  |          -- Set a line breakpoint. \n \
  \bl |          -- Set a label breakpoint. \n \
  \bp |          -- Show current breakpoints.  \n"


-- | Processes a command from main and calls the appropriate
--   functions to deal with them.
processCmd :: VMState -> String -> IO VMState
processCmd s cmd
  | cmd `elem` ["q", "quit"] = exitSuccess
  | cmd `elem` ["h", "help"] = putStrLn helpText >> return s
  | cmd `elem` ["b", "breakpoint"] = setBreakLine s
  | cmd `elem` ["c", "continue"] = continueInstruction s
  | cmd `elem` ["n", "next"] = nextInstruction s
  | cmd `elem` ["r", "reg", "registers"] = showRegisters s >> return s
  | cmd `elem` ["p", "prog"] = showProgram s >> return s
  | cmd `elem` ["m", "mem", "memory"] = showMemory s >> return s
  | cmd `elem` ["l", "lbl", "labels"] = showLabels s >> return s
  | cmd `elem` ["bp"] = showBreakpoints s >> return s
  | cmd `elem` ["bl"] = setBreakLabel s
  | otherwise =
      putStrLn "Command not recognized. Type help or h for help." >> return s

showBreakpoints :: VMState -> IO ()
showBreakpoints s = do
  putStrLn "Breakpoints"
  putStrLn . show . LC4VM.brks $ s

showLabels :: VMState -> IO ()
showLabels s = do
  putStrLn "Labels"
  putStrLn . show . LC4VM.lbls $ s

showMemory :: VMState -> IO ()
showMemory s = do
  putStrLn "Memory"
  putStrLn . unlines . (fmap show) . M.toList . memory $ s

showProgram :: VMState -> IO ()
showProgram s = do
  putStrLn "Program Counter"
  putStrLn . show . pc $ s
  putStrLn (case M.lookup (pc s) (LC4VM.prog s) of
    Nothing   -> "No valid insn"
    Just insn -> display insn)

showRegisters :: VMState -> IO ()
showRegisters s = do
  putStrLn "Regfile"
  putStrLn . unlines $ fmap show $ M.toList $ regFile s
  putStrLn . display $ cc s

continueInstruction :: VMState -> IO VMState
continueInstruction s = return $ execState continue s

nextInstruction :: VMState -> IO VMState
nextInstruction s = return $ execState nextStep s

-- Takes in a PC and sets the breakpoint
setBreakLine :: VMState -> IO VMState
setBreakLine s = do
  putStr "Setting Line Breakpoint ~>"
  hFlush stdout
  bkpt <- getLine
  addBreakLine s bkpt

setBreakLabel :: VMState -> IO VMState
setBreakLabel s = do
  putStr "Setting Label Breakpoint ~>"
  hFlush stdout
  bkpt <- getLine
  addBreakLabel s bkpt

addBreakLabel :: VMState -> String -> IO VMState
addBreakLabel s bkpt = do
  let l = M.lookup (read bkpt::Label) (LC4VM.lbls s) in
    case l of
      Just line -> return $ s { LC4VM.brks = (Set.insert line (LC4VM.brks s)) }
      Nothing   -> return s
      --return $ s { LC4VM.brks = (Set.insert line (LC4VM.brks s)) }

-- takes in the VMState and actually adds the breakpoint to the bkpt map
addBreakLine :: VMState -> String -> IO VMState
addBreakLine s bkpt =
  let line = read bkpt::Int in
  return $ s { LC4VM.brks = (Set.insert line (LC4VM.brks s)) }

repl :: VMState -> IO ()
repl s = do
  putStr prompt
  hFlush stdout
  cmd <- getLine
  putStrLn $ "You typed " ++ cmd ++ ". "
  s' <- processCmd s (map toLower cmd)
  repl s'

mainHelp :: String
mainHelp = "LC4 Interpreter. Usage: lc4sim <filename.asm>"

parseLinesFromFile :: String -> IO ([Either ParseError Line])
parseLinesFromFile filename = do
  handle    <- openFile filename ReadMode
  contents  <- hGetContents handle
  -- putStrLn $ show $ lines contents
  return $ fmap (\line -> parse lineP line) (lines contents)

checkParsedLines :: ([Either ParseError Line]) -> IO [Line]
checkParsedLines [] = return []
checkParsedLines (Left err : _ls) = do
  putStrLn $ "Parser failed:\n\t" ++ err
  exitFailure
checkParsedLines (Right l : ls) = do
  ls <- checkParsedLines ls
  return $ l : ls

run :: String -> IO ()
run filename = do
  prelines  <- parseLinesFromFile filename
  lines     <- checkParsedLines prelines
  putStrLn . unlines $ fmap show lines
  putStrLn welcomeMsg
  repl $ load lines

-- | Parse file from command arguments and run the REPL.
main :: IO ()
main = do 
  args <- getArgs
  case args of
    (fname:[])  -> run fname
    _           -> putStrLn "Run with [filename] argument, type --h for help."

test :: IO ()
test = run "fib.asm"
