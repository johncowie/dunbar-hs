module Store.State (
  store
, retrieve
, retrieveAll
, update
, delete
, store'
, retrieve'
, retrieveAll'
, update'
, delete'
) where

import Control.Monad.State (State)
import qualified Control.Monad.State as ST
import Store.Id (nextId)
import Utils.List (maybeHead, updateAt)
-- import Control.Monad.Trans.Class (lift)

type Records a = [(String, a)]


-- TODO using lenses to lens into the shape of the state would be super useful
store :: a -> State (Records a) (Either String ())
store x = do
  records <- ST.get
  case nextId records of
    (Left err) -> return (Left err)
    (Right i) -> Right <$> ST.put ((i, x):records)

store' :: a -> State (b, Records a) (Either String ())
store' x = do
  (b, records) <- ST.get
  case nextId records of
    (Left err) -> return (Left err)
    (Right i) -> Right <$> ST.put (b, (i, x):records)

retrieve :: String -> State (Records a) (Either String (Maybe a))
retrieve s = do
  records <- ST.get
  return . Right . maybeHead . map snd . filter ((== s) . fst) $ records

retrieve' :: String -> State (b, Records a) (Either String (Maybe a))
retrieve' s = do
  (b, records) <- ST.get
  return . Right . maybeHead . map snd . filter ((== s) . fst) $ records

retrieveAll :: State (Records a) (Either String (Records a))
retrieveAll = Right <$> ST.get

retrieveAll' :: State (b, Records a) (Either String (Records a))
retrieveAll' = Right <$> snd <$> ST.get

update :: String -> (a -> a) -> State (Records a) (Either String ())
update s f = do
  records <- ST.get
  Right <$> ST.put (updateAt ((== s) . fst) (fmap f) records)

update' :: String -> (a -> a) -> State (b, Records a) (Either String ())
update' s f = do
  (b, records) <- ST.get
  Right <$> ST.put (b, updateAt ((== s) . fst) (fmap f) records)

delete :: String -> State (Records a) (Either String (Maybe a))
delete s = do
  records <- ST.get
  let deleted = maybeHead $ map snd $ filter ((== s) . fst) records
  ST.put (filter ((/= s) . fst) records)
  return $ Right deleted

delete' :: String -> State (b, Records a) (Either String (Maybe a))
delete' s = do
  (b, records) <- ST.get
  let deleted = maybeHead $ map snd $ filter ((== s) . fst) records
  ST.put (b, filter ((/= s) . fst) records)
  return $ Right deleted
