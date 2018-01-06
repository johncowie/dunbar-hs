module Utils.Console (
  Console
, monad
, input
, output
, stop
, run
, runInputIO
, runState
)
where

{-# LANGUAGE FlexibleInstances #-}

import Text.Read (readEither)
import Utils.Cli (Cli(getALine, putALine))
import qualified Control.Monad.State as ST
import System.Console.Haskeline (InputT, runInputT, defaultSettings, MonadException)
import Control.Monad.State (State)
import Control.Monad (liftM, liftM2)
-- import Control.Applicative (liftA)


data Console m a =   Input (String -> Console m a)
                   | Output String (Console m a)
                   | DoMonad (m (Console m a))
                   | Return a
                   | Stop

output = Output
input = Input
stop = Stop
monad = DoMonad

continue :: (Cli m) => Console m a -> m (Console m a)
continue (Input f) = maybe stop f <$> getALine
continue (Output s c) = putALine s >> return c
continue (DoMonad m) = m
continue x = return x

run :: (Cli m) => Console m a -> m ()
run (Stop) = return ()
run (Return a) = return ()
run c = continue c >>= run

instance (Cli m) => Functor (Console m) where
  fmap f (Return a) = return (f a)
  fmap f (Stop)     = stop
  fmap f c          = monad $ fmap f <$> continue c

instance (Cli m) => Applicative (Console m) where
  (<*>) (Return f) a = f <$> a
  (<*>) (Stop) a     = stop
  (<*>) c a          = monad $ liftM2 (<*>) (continue c) (return a)
  pure               = Return

instance (Cli m) => Monad (Console m) where
  (>>=) (Return a) f = f a
  (>>=) (Stop) f = stop
  (>>=) c f = monad $ liftM2 (>>=) (continue c) (return f)

-- TODO make traversable instance?



-- FIXME move this stuff out
type IOState = ([String], [String])

runInputIO :: (MonadException m) => Console (InputT m) a -> m ()
runInputIO = runInputT defaultSettings . run

runState :: x -> [String] -> Console (State (IOState, x)) a -> (IOState, x)
runState startState inputs startStep = snd $ ST.runState (run startStep) ((inputs, []), startState)
