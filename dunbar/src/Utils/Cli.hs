{-# LANGUAGE FlexibleInstances #-}

module Utils.Cli (
  -- runConsole
  runInputIO
, run
, runState
, step
, mStep
, inputStep
, withOutput
, terminate
, ConsoleStep
, Cli
, CliState
)
where

import System.Console.Haskeline (InputT, runInputT, getInputLine, outputStrLn, defaultSettings, MonadException)
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State as ST
import Control.Monad.State (State, StateT, runStateT)
import Utils.List (maybeHead, maybeTail)
import Store.File (SingleFileIO)
import Control.Monad.Trans (lift)

data NextStep m = InputStep  (String -> ConsoleStep m) |
                  Transition (ConsoleStep m) |
                  TransitionM (m (ConsoleStep m)) |
                  Termination

type Output = Maybe String

data ConsoleStep m = ConsoleStep Output (NextStep m)

printAnyOutput :: (Cli m) => Maybe String -> m ()
printAnyOutput (Just s) = putALine s
printAnyOutput Nothing = return ()

continue :: (Cli m) => NextStep m -> m (ConsoleStep m)
continue (InputStep f) = do
  iM <- getALine
  return $ maybe (ConsoleStep Nothing Termination) f iM
continue (Transition s) = return s
continue (TransitionM s) = s
continue Termination = return (ConsoleStep Nothing Termination)

process :: (Cli m) => ConsoleStep m -> m (ConsoleStep m)
process (ConsoleStep s nextStep) = do
  printAnyOutput s
  continue nextStep

inputStep :: (String -> ConsoleStep m) -> ConsoleStep m
inputStep = ConsoleStep Nothing . InputStep

step :: ConsoleStep m -> ConsoleStep m
step = ConsoleStep Nothing . Transition

mStep :: m (ConsoleStep m) -> ConsoleStep m
mStep = ConsoleStep Nothing . TransitionM

terminate :: ConsoleStep m
terminate = ConsoleStep Nothing Termination

addOutput :: String -> Maybe String -> Maybe String
addOutput s = Just . maybe s ((++) s)

withOutput :: String -> ConsoleStep m -> ConsoleStep m
withOutput s (ConsoleStep o nextStep) = ConsoleStep (addOutput s o) nextStep


class Monad m => Cli m where
  putALine :: String -> m ()
  getALine :: m (Maybe String)

instance Cli IO where
  putALine = putStrLn
  getALine = Just <$> getLine

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

cliLoop :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m ()
cliLoop p f i
  | p i = return ()
  | otherwise = do
      a <- f i
      cliLoop p f a

hasTerminated :: ConsoleStep m -> Bool
hasTerminated (ConsoleStep _ Termination) = True
hasTerminated _ = False

run :: (Cli m) => ConsoleStep m -> m ()
run = cliLoop hasTerminated process

runInputIO :: (MonadException m) => ConsoleStep (InputT m) -> m ()
runInputIO = runInputT defaultSettings . run

runState :: x -> [String] -> ConsoleStep (State (IOState, x)) -> (IOState, x)
runState startState inputs startStep = snd $ ST.runState (run startStep) ((inputs, []), startState)
