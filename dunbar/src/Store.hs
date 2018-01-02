{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Store (
  Store(..)
) where

import qualified Store.File as F
import qualified Store.State as S
import System.Console.Haskeline (InputT)
import Control.Monad.State (State, StateT)
import Control.Monad.Trans.Class (lift)
import Data.Typeable (Typeable)
import Data.Default (Default)

class Monad m => Store m a where
  store :: a -> m (Either String ())
  retrieve :: String -> m (Either String (Maybe a))
  retrieveAll :: m (Either String [(String, a)])
  update :: String -> (a -> a) -> m (Either String ())
  delete :: String -> m (Either String (Maybe a))

instance (Store m a, Read a, Show a) => Store (InputT m) a where
  store = lift . store
  retrieve = lift . retrieve
  retrieveAll = lift $ retrieveAll
  update s = lift . update s
  delete = lift . delete

instance (Typeable a, Default a, Read a, Show a) => Store F.SingleFileIO a where
  store = F.store
  retrieve = F.retrieve
  retrieveAll = F.retrieveAll
  update = F.update
  delete = F.delete

instance Store (State [(String, a)]) a where
  store = S.store
  retrieve = S.retrieve
  retrieveAll = S.retrieveAll
  update = S.update
  delete = S.delete

instance Store (State (b, [(String, a)])) a where
  store = S.store'
  retrieve = S.retrieve'
  retrieveAll = S.retrieveAll'
  update = S.update'
  delete = S.delete'
