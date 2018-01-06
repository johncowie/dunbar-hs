{-# LANGUAGE FlexibleInstances #-}

module Utils.Cli (
  CliState
)
where

import Consolation.Cli (Cli(..))
import System.Console.Haskeline (InputT, runInputT, getInputLine, outputStrLn, MonadException)
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State as ST
import Control.Monad.State (State, StateT, runStateT)
import Utils.List (maybeHead, maybeTail)
import Store.File (SingleFileIO)
import Control.Monad.Trans (lift)

instance (MonadException m) => Cli (InputT m) where
  putALine = outputStrLn
  getALine = getInputLine ""

instance Cli SingleFileIO where
  putALine = lift . putStrLn
  getALine = lift getALine

type IOState = ([String], [String])

type CliState x = State (IOState, x)

instance Cli (State IOState) where
  putALine s = ST.modify (addToOutputs s)
              where addToOutputs s (inputs, outputs) = (inputs, s:outputs)
  getALine = do
    (is, os) <- ST.get
    ST.put (maybeTail is, os)
    return (maybeHead is)

instance Cli (State (IOState, b)) where
  putALine s = ST.modify (addToOutputs s)
               where addToOutputs s ((is, os), a) = ((is, s:os), a)
  getALine = do
    ((is, os), x) <- ST.get
    ST.put ((maybeTail is, os), x)
    return (maybeHead is)
