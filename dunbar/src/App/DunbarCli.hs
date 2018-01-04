{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.DunbarCli (
  start
) where

-- import           Utils.Cli (runInputIO)
import           Utils.Console (Console, output, input, monad, stop, runInputIO)
import qualified Store as F
import           Store.File (runForFile)
import           Data.Text (strip, toLower, pack)
import           Data.Friend (Friend, newFriend, showName)
import           Data.List (sortOn)
import qualified App.Messages as M

requestOption :: (F.Store m Friend) => Console m ()
requestOption = monad $ do
  friends :: Either String [(String, Friend)] <- F.retrieveAll
  case friends of
    (Left err) -> return $ output err stop
    (Right _) -> return $ output M.mainMenu readOption

viewFriends :: (F.Store m Friend) => Console m ()
viewFriends = monad $ do
  friends <- F.retrieveAll
  case friends of
    (Left err) -> return $ output err stop
    (Right friends) -> return $ output (showFriends friends) requestOption
  where showTuple (i, r) = i ++ ": " ++ showName r
        showFriends [] = "<no-friends>"
        showFriends rs = unlines $ map showTuple $ sortOn fst rs

requestIdToDelete :: (F.Store m Friend) => Console m ()
requestIdToDelete = output "Enter ID of friend to delete:" readIdToDelete

requestIdToShow :: (F.Store m Friend) => Console m ()
requestIdToShow = output M.enterFriendId readIdToShow

readIdToDelete :: (F.Store m Friend) => Console m ()
readIdToDelete = input $ \s -> monad (deleteFriend s)

readIdToShow :: (F.Store m Friend) => Console m ()
readIdToShow = input $ \s -> monad (showFriend s)

showFriend :: (F.Store m Friend) => String -> m (Console m ())
showFriend i = do
  friend <- F.retrieve i
  case friend of
    (Left err) -> return $ output err stop
    (Right (Just friend)) -> return $ output (showName friend) requestOption
    (Right Nothing) -> return $ output (M.friendDoesNotExist i) requestOption


deleteFriend :: (F.Store m Friend) => String -> m (Console m ())
deleteFriend s = do
  deleted <- F.delete s
  case deleted of
    (Right (Just friend)) -> return $ output ("Deleted: " ++ showName friend) requestOption
    (Right Nothing) -> return $ output ("Couldn't find friend with ID: " ++ s) requestOption
    (Left err) -> return $ output err requestOption

readOption :: (F.Store m Friend) => Console m ()
readOption = input $ \s -> case (toLower . strip . pack $ s) of
  "n" -> requestFirstName
  "v" -> viewFriends
  "s" -> requestIdToShow
  "d" -> requestIdToDelete
  "q" -> stop
  _ -> output "Invalid Option: please try again" requestOption

requestFirstName :: (F.Store m Friend) => Console m ()
requestFirstName = output M.enterFirstname readFirstName

readFirstName :: (F.Store m Friend) => Console m ()
readFirstName = input $ \firstName -> do
  case firstName of
    "" -> output M.emptyFirstname requestFirstName
    _ -> output ("You entered: " ++ firstName) (requestLastName firstName)

requestLastName :: (F.Store m Friend) => String -> Console m ()
requestLastName firstName = output M.enterLastname (readLastName firstName)

readLastName :: (F.Store m Friend) => String -> Console m ()
readLastName firstName = input $ \lastName -> do
  case lastName of
    "" -> output M.emptyLastname (requestLastName firstName)
    _ -> output ("You entered: " ++ lastName) (requestNote firstName lastName)

requestNote :: (F.Store m Friend) => String -> String -> Console m ()
requestNote firstName lastName = output M.enterNote (readNote firstName lastName)

readNote :: (F.Store m Friend) => String -> String -> Console m ()
readNote firstName lastName = input $ \note -> do
  monad (storeFriend (newFriend firstName lastName (notes note)))
  where notes "" = []
        notes n = [n]

storeFriend :: (F.Store m Friend) => Friend -> m (Console m ())
storeFriend f = do
  result <- F.store f
  case result of
    (Left err) -> return $ output err requestOption
    (Right _) -> return requestOption

start :: (F.Store m Friend) => Console m ()
start = requestOption

main' :: String -> IO ()
main' s = runForFile s (runInputIO requestOption)

main :: IO ()
main = main' "resources.txt"
