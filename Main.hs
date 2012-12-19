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
import LC4Draw

-- | Processes a command from main and calls the appropriate
--   functions to deal with them.
processCmd :: VMState -> String -> IO VMState
processCmd s cmd
  -- | cmd `elem` ["b", "breakpoint"] = setBreakLine s
  -- | cmd `elem` ["bl"] = setBreakLabel s
  -- | cmd `elem` ["rs"] = resetVM s
  | cmd `elem` ["sc"] = setScript s >> return s
  | otherwise = return s

{-
-- | Takes in a PC and sets the breakpoint
setBreakLine :: VMState -> IO VMState
setBreakLine s = do
  putStr "Setting Line Breakpoint ~>"
  hFlush stdout
  bkpt <- getLine
  addBreakLine s bkpt
-}

-- | Takes a script as input, then calls runScript
setScript :: VMState -> IO ()
setScript s = do
  putStr "Setting Script ~>"
  hFlush stdout
  script <- getLine
  runScript s script

-- | Reverts back to the state of the virtual machine POST-script
-- Once a script is loaded, the original reset state is modified
resetVM :: VMState -> IO VMState
resetVM s = return $ execState reset s

-- | Breakpoints for labels only
setBreakLabel :: VMState -> IO VMState
setBreakLabel s = do
  putStr "Setting Label Breakpoint ~> "
  hFlush stdout
  bkpt <- getLine
  addBreakLabel s bkpt

addBreakLabel :: VMState -> String -> IO VMState
addBreakLabel s bkpt = do
  let l = M.lookup (read bkpt::Label) (LC4VM.lbls s) in
    case l of
      Just line -> return $ s { LC4VM.brks = (Set.insert line (LC4VM.brks s)) }
      Nothing   -> return s

{-
-- | takes in the VMState and actually adds the breakpoint to the bkpt map
addBreakLine :: VMState -> String -> IO VMState
addBreakLine s bkpt =
  let line = read bkpt::Int in
  return $ s { LC4VM.brks = (Set.insert line (LC4VM.brks s)) }
-}

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

parseScriptFromFile :: String -> IO ([Either ParseError ScriptInsn])
parseScriptFromFile filename = do
  handle    <- openFile filename ReadMode
  contents  <- hGetContents handle
  return $ fmap (\line -> parse scriptP line) (lines contents)

checkParsedScript :: ([Either ParseError ScriptInsn]) -> IO [ScriptInsn]
checkParsedScript [] = return []
checkParsedScript (Left err : _ls) = do
  putStrLn $ "Parser failed:\n\t" ++ err
  exitFailure
checkParsedScript (Right l : ls) = do
  ls <- checkParsedScript ls
  return $ l : ls

runScript :: VMState -> String -> IO ()
runScript s filename = do
  prelines <- parseScriptFromFile filename
  lines    <- checkParsedScript prelines
  putStrLn . unlines $ fmap show lines 
  runDebugger $ loadScript s lines

run :: String -> IO ()
run filename = do
  prelines  <- parseLinesFromFile filename
  lines     <- checkParsedLines prelines
  putStrLn . unlines $ fmap show lines
  putStrLn welcomeMsg
  runDebugger $ load lines

-- | Parse file from command arguments and run the REPL.
main :: IO ()
main = do
  args <- getArgs
  case args of
    (fname:[])  -> run fname
    _           -> putStrLn "Run with [filename] argument, type --h for help."

test :: IO ()
test = run "fib.asm"
