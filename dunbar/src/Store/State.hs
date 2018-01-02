module Store.State (
  store
, retrieve
, retrieveAll
, update
, delete
) where

import Control.Monad.State (State)
import qualified Control.Monad.State as ST
import Store.Id (nextId)
import Utils.List (maybeHead, updateAt)

type Records a = [(String, a)]


-- TODO using lenses to lens into the shape of the state would be super useful

store :: (x -> Records a) -> (Records a -> x -> x) -> a -> State x (Either String ())
store getter setter x = do
  st <- ST.get
  let records = (getter st)
  case nextId records of
    (Left err) -> return (Left err)
    (Right i) -> Right <$> ST.put (setter ((i, x):records) st)

retrieve :: (x -> Records a) -> (Records a -> x -> x) -> String -> State x (Either String (Maybe a))
retrieve getter setter s = do
  st <- ST.get
  return . Right . maybeHead . map snd . filter ((== s) . fst) . getter $ st

retrieveAll :: (x -> Records a) -> (Records a -> x -> x) -> State x (Either String (Records a))
retrieveAll getter setter = Right <$> getter <$> ST.get

update :: (x -> Records a) -> (Records a -> x -> x) -> String -> (a -> a) -> State x (Either String ())
update getter setter s f = do
  st <- ST.get
  Right <$> ST.put (setter (updateAt ((== s) . fst) (fmap f) (getter st)) st)

delete :: (x -> Records a) -> (Records a -> x -> x) -> String -> State x (Either String (Maybe a))
delete getter setter s = do
  st <- ST.get
  let deleted = maybeHead $ map snd $ filter ((== s) . fst) (getter st)
  ST.put (setter (filter ((/= s) . fst) (getter st)) st)
  return $ Right deleted
