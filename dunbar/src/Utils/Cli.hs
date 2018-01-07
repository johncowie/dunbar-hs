{-# LANGUAGE FlexibleInstances #-}

module Utils.Cli (
)
where

import Consolation.Cli (Cli(..))
import System.Console.Haskeline (InputT, runInputT, getInputLine, outputStrLn, MonadException)
import Store.File (SingleFileIO)
import Control.Monad.Trans (lift)

instance (MonadException m) => Cli (InputT m) where
  putALine = outputStrLn
  getALine = getInputLine ""

instance Cli SingleFileIO where
  putALine = lift . putStrLn
  getALine = lift getALine
