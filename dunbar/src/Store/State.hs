module Store.State (
  store
, retrieve
, retrieveAll
, update
, delete
) where

import Control.Monad.State (State, StateT)
import qualified Control.Monad.State as ST
import Store.Id (nextId)
import Utils.List (maybeHead, updateAt)

type Records a = [(String, a)]

store :: (Monad m) => a -> StateT (Records a) m (Either String ())
store x = do
  records <- ST.get
  case nextId records of
    (Left err) -> return (Left err)
    (Right i) -> Right <$> ST.put ((i, x):records)

retrieve :: (Monad m) => String -> StateT (Records a) m (Either String (Maybe a))
retrieve s = do
  records <- ST.get
  return . Right . maybeHead . map snd . filter ((== s) . fst) $ records

retrieveAll :: (Monad m) => StateT (Records a) m (Either String (Records a))
retrieveAll = Right <$> ST.get

update :: (Monad m) => String -> (a -> a) -> StateT (Records a) m (Either String ())
update s f = do
  records <- ST.get
  Right <$> ST.modify (updateAt ((== s) . fst) (fmap f))

delete :: (Monad m) => String -> StateT (Records a) m (Either String (Maybe a))
delete s = do
  records <- ST.get
  let deleted = maybeHead $ map snd $ filter ((== s) . fst) records
  ST.modify (filter ((/= s) . fst))
  return $ Right deleted
