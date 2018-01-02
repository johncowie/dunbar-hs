{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module StoreSpec (
  main
) where

import System.Directory (removeFile)
import Store (Store(store, retrieve, retrieveAll, update, delete))
import Store.File (SingleFileIO, runForFile)
import Test.Hspec (SpecWith, hspec, describe, it, shouldBe, Expectation, pending, pendingWith, shouldReturn, before_, after_, around)
import Control.Monad.State (runState, State)
import Data.Typeable (Typeable)
import Data.Default (Default(def))

data Fruit = Apple | Banana | Orange | Pear deriving (Eq, Read, Show, Typeable)

instance Default Fruit where
  def = Apple


testSuite :: (Store m Fruit) => (forall a. m a -> IO a) -> String -> SpecWith ()
testSuite runner description = do
  describe description $ do
    it "should return empty list if nothing has been saved" $ do
      runner retrieveAll `shouldReturn` (Right [] :: Either String [(String, Fruit)])

    it "should return stored items with incrementing id" $ do
      let result = do
                    store Apple
                    store Banana
                    store Orange
                    retrieveAll
      runner result `shouldReturn` (Right [("2", Orange), ("1", Banana), ("0", Apple)])

    it "should allow retrieval of single item" $ do
      let state = do
                   store Apple
                   store Orange
      runner (state >> retrieve "0") `shouldReturn` Right (Just Apple)

    it "should return nothing for non-existant id" $ do
      let state = store Banana
      runner (state >> retrieve "1") `shouldReturn` Right (Nothing :: Maybe Fruit)

    it "should be able to delete record by ID" $ do
      let state = do
                    store Pear
                    store Apple
                    store Banana
                    deleted <- delete "1"
                    remaining <- retrieveAll
                    return (deleted, remaining)
      runner state `shouldReturn` (Right (Just Apple), Right [("2", Banana), ("0", Pear)])

emptyFruitState :: [(String, Fruit)]
emptyFruitState = []

stateToIO :: forall s a. s -> State s a -> IO a
stateToIO initial st = return $ fst $ runState st initial

main :: IO ()
main = do
  hspec $ testSuite (stateToIO emptyFruitState) "State store"
  let testFile = "testFile.txt"
  hspec $ before_ (writeFile testFile "") $ after_ (removeFile testFile) $ testSuite (runForFile testFile) "File store"
