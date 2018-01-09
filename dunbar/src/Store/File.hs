{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Store.File (
  store
, retrieveAll
, retrieve
, update
, delete
, runForFile
, SingleFileIO
) where

import Prelude hiding (readFile)
import Data.ByteString (readFile)
import Data.ByteString.Char8 (unpack)
import Data.List (sort)
import Utils.List (maybeHead, updateAt)
import qualified Text.Read as T
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Store.Id (nextId)
import Utils.Read (readEither) 
import Utils.ToMove (addErrorPrefix)
import Data.Typeable (Typeable)

-- TODO move these types out
newtype FileLocation = FileLocation {fileLocation :: String}
type SingleFileIO = ReaderT FileLocation IO

runForFile :: String -> SingleFileIO a -> IO a
runForFile filePath r = runReaderT r (FileLocation filePath)

store :: forall a. (Typeable a, Read a, Show a) => a -> SingleFileIO (Either String ())
store record = runExceptT $ do
  fileName <- fileLocation <$> ask
  records :: [(String, a)] <- ExceptT retrieveAll
  recordId <- ExceptT $ return $ nextId records
  ExceptT $ lift $ replaceAll fileName ((:) (recordId, record))

retrieve :: (Typeable a, Read a) => String -> SingleFileIO (Either String (Maybe a))
retrieve recordId = runExceptT $ do
  rows <- ExceptT retrieveAll
  return . maybeHead . map snd . filter ((== recordId) . fst) $ rows

retrieveAll :: (Typeable a, Read a) => SingleFileIO (Either String [(String, a)])
retrieveAll = do
  fileName <- fileLocation <$> ask
  lift $ sequence . map readEither . lines . unpack  <$> readFile fileName

update :: (Typeable a, Read a, Show a) => String -> (a -> a) -> SingleFileIO (Either String ())
update recordId f = do
  fileName <- fileLocation <$> ask
  lift $ replaceAll fileName $ updateAt (eqId recordId) (fmap f)

delete :: forall a. (Typeable a, Read a, Show a) => String -> SingleFileIO (Either String (Maybe a))
delete recordId = runExceptT $ do
  fileName <- fileLocation <$> ask
  records <- ExceptT retrieveAll
  let notEq :: (String, a) -> Bool = (not . eqId recordId)
  ExceptT $ lift $ replaceAll fileName $ filter notEq
  return . maybeHead . map snd . filter (eqId recordId) $ records


--- Utils

eqId :: String -> (String, a) -> Bool
eqId s = (== s) . fst

runExceptTPrefixed :: (Monad m) => String -> ExceptT String m a -> m (Either String a)
runExceptTPrefixed pfx = (fmap (addErrorPrefix pfx)) . runExceptT

processLines :: (Typeable a, Read a, Show a) => ([a] -> [a]) -> String -> Either String String
processLines f s = addErrorPrefix "processLines: " $ do
  objects <- sequence . map readEither . lines $ s
  return . unlines . map show . f $ objects

replaceAll :: (Typeable a, Read a, Show a) => FilePath -> ([a] -> [a]) -> IO (Either String ())
replaceAll fp f = addErrorPrefix "replaceAll: " <$> replaceFile fp (processLines f)

liftError :: Either String (IO ()) -> IO (Either String ())
liftError (Left err) = return (Left err)
liftError (Right io) = Right <$> io

replaceFile :: FilePath -> (String -> Either String String) -> IO (Either String ())
replaceFile fp f = do
  contents <- readFile fp
  liftError $ writeFile fp <$> (f (unpack contents))
