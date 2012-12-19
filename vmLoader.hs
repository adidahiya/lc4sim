-- This content is released under the (Link Goes Here) MIT License.

{-# OPTIONS_GHC -Wall #-}

module VMLoader where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import LC4VM

data LoaderMode = START | CODE | DATA deriving Eq

-- | Used to generate our internal representation of the LC4 VM after a file is
-- parsed into a list of lines.
data LoaderState = L { mode :: LoaderMode
                     , addr :: Int
                     , prog :: Program
                     , lbls :: Labels
                     , brks :: Breakpoints
                     , mem  :: Memory }

blankRegFile :: Map Register Int
blankRegFile = M.fromList [ (R0,0),
                            (R1,0),
                            (R2,0),
                            (R3,0),
                            (R4,0),
                            (R5,0),
                            (R6,0),
                            (R7,0) ]

initial :: LoaderState
initial = L START 0 M.empty M.empty S.empty M.empty

load :: [Line] -> VMState
load ls' = convert $ execState (loadAll ls') initial where

  convert :: LoaderState -> VMState
  convert (L _ _a p lbls' brks' mem') = 
    let start' = VM Nothing p lbls' blankRegFile mem' brks' 0 CC_Z False in
    start' { start = Just start' }

  loadAll :: [Line] -> State LoaderState ()
  loadAll = foldl (\ls l -> ls >> (loadLine l)) (return ())

loadScript :: VMState -> [ScriptInsn] -> VMState
loadScript s insns = 

  let s' = execState (loadAll insns) s in 
  s' { start = Just s' }

  where

  loadAll :: [ScriptInsn] -> State VMState ()
  loadAll = foldl (\st insn -> st >> (loadScriptLine insn)) (return ())
 
loadScriptLine :: ScriptInsn -> State VMState ()
loadScriptLine (SETPC _ val) = do
  s <- get
  put $ s { pc = val }

loadScriptLine (SETADDR addr' val) = do
  s <- get
  let mem' = memory s
  put $ s { memory = (M.insert addr' val mem') }

loadScriptLine (SETREG reg val) = do
  s <- get
  let regFile' = regFile s
  put $ s { regFile = (M.insert reg val regFile') }

loadScriptLine (BREAKL lbl) = do
  s <- get
  let line = M.lookup lbl (LC4VM.lbls s)
  case line of
    Nothing -> put $ s
    Just line' -> put $ s { LC4VM.brks = S.insert line' (LC4VM.brks s) }

loadScriptLine (BREAKN line) = do
  s <- get
  put $ s { LC4VM.brks = S.insert line (LC4VM.brks s) }

loadScriptLine _ = error "Hit bad case while pattern matching script insns."

addLabel :: Label -> State LoaderState ()
addLabel lbl = do
  s <- get
  put $ s { VMLoader.lbls = 
              M.insert lbl (addr s) (VMLoader.lbls s) }

align :: Int -> Int
align i = if i `mod` 16 == 0
          then i
          else i + (16 - i `mod` 16)

incrAddr :: State LoaderState ()
incrAddr = get >>= (\s -> put $ s { VMLoader.addr =
                                      (VMLoader.addr s + 1) })

-- | Look at each parsed line and modify the loader state as necessary.
loadLine :: Line -> State LoaderState ()
loadLine Empty        = return ()
loadLine (Label lbl)  = addLabel lbl
loadLine (Insn insn)  = do
  s <- get
  if (mode s) /= CODE
  then error "insn in non-code section"
  else do put $ s { VMLoader.prog =
                      M.insert (VMLoader.addr s) insn (VMLoader.prog s) }
          incrAddr

-- Change the current mode of the loader to DATA.
loadLine (Dir D_DATA (Just _)) = error "labeled .DATA directive"
loadLine (Dir D_DATA Nothing)  =
  get >>= (\s -> put s { mode = DATA })

-- Change the current mode of the loader to CODE.
loadLine (Dir D_CODE (Just _)) = error "labeled .CODE directive"
loadLine (Dir D_CODE Nothing)  =
  get >>= (\s -> put s { mode = CODE})

loadLine (Dir (D_ADDR _    ) (Just _)) = error "labeled .ADDR directive"
loadLine (Dir (D_ADDR addr') Nothing)  =
  get >>= (\s -> put s { VMLoader.addr = addr'})

-- Update the addr of the loader to be aligned to a 16-word boundary.
loadLine (Dir D_FALIGN (Just _)) = error "labeled .FALIGN directive"
loadLine (Dir D_FALIGN Nothing) =
  get >>= (\s -> put s { VMLoader.addr =
                           align (VMLoader.addr s) })

-- Simple data entry into loader memory state.
loadLine (Dir (D_FILL i) maybeLabel) = do
  get >>= (\s -> put $ s { VMLoader.mem =
                              M.insert (VMLoader.addr s) i (VMLoader.mem s) })
  (case maybeLabel of
    Just lbl  -> addLabel lbl
    Nothing   -> return ())
  incrAddr

loadLine (Dir (D_BLKW i) maybeLabel) = do
  (case maybeLabel of
    Just lbl  -> addLabel lbl
    Nothing   -> return ())
  get >>= (\s -> put s {VMLoader.addr = (VMLoader.addr s) + i})

-- TODO: support these
loadLine (Dir (D_CONST _) _)  = error "unsupported"
loadLine (Dir (D_UCONST _) _) = error "unsupported"
