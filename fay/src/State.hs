{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State (State, get, put, state, runState, evalState, execState) where

import Language.Fay.Prelude
import Language.Fay.FFI
import Control.Monad

data State s a = S { runState :: s -> (a, s) }

state :: (s -> (a,s)) -> State s a
state = S

instance Monad (State s) where
  return x   = S (\s -> (x, s))
  st >>= f   = S (\s -> let (x, s') = runState st s 
                          in runState (f x) s')

evalState :: State s a -> s -> a
evalState s = fst . runState s

execState :: State s a -> s -> s
execState s  = snd . runState s

get :: State s s
get = S (\s -> (s, s))

put :: s -> State s ()
put s' = S (\s -> ((), s'))

