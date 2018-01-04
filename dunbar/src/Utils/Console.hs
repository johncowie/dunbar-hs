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
continue (Return a) = return (return a)
continue (Stop) = return stop

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
  (<*>) (Stop) a = stop
  (<*>) c a = monad $ do
    nextC <- continue c
    return $ nextC <*> a
  pure = Return

instance (Cli m) => Monad (Console m) where
  (>>=) (Return a) f = f a
  (>>=) (Stop) f = stop
  (>>=) c f = monad $ do
    nextC <- continue c
    return $ nextC >>= f


-- FIXME move this stuff out
type IOState = ([String], [String])

runInputIO :: (MonadException m) => Console (InputT m) a -> m ()
runInputIO = runInputT defaultSettings . run

runState :: x -> [String] -> Console (State (IOState, x)) a -> (IOState, x)
runState startState inputs startStep = snd $ ST.runState (run startStep) ((inputs, []), startState)

-- -- example..
-- branch :: Either a b -> (b -> Console m b) -> Console m b -> Console m b
-- branch (Left _) succ err = err
-- branch (Right v) succ err = succ v
--
-- inputInt :: (Cli m) => Console m Int
-- inputInt = output "Enter an int" $ Input $ \s ->
--   branch (readEither s)
--     return
--     ( output "Error reading int - try again"
--     $ inputInt )
--
-- showSum :: (Cli m) => Int -> Console m ()
-- showSum int = DoMonad $ do
--   putALine $ show int
--   return stop
--
-- inputSum :: (Cli m) => Console m Int
-- inputSum = do
--   a <- inputInt
--   b <- inputInt
--   c <- inputInt
--   return $ a + b + c
--
-- start :: (Cli m) => Console m ()
-- start = Output "Let's sum some numbers!" inputSum >>= showSum
--
-- main :: IO ()
-- main = run start
