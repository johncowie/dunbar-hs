{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module App.DunbarCli (
  start
) where

-- import           Utils.Cli (runInputIO)
import           Utils.Console (Console, output, input, monad, stop, runInputIO)
import           Utils.Cli (Cli)
import qualified Store as F
import           Store.File (runForFile)
import           Data.Text (strip, toLower, pack)
import           Data.Friend (Friend)
import qualified Data.Friend as Friend
import           Data.List (sortOn)
import qualified App.Messages as M

type DunbarCli m a = (Cli m, F.Store m Friend) => Console m a

requestOption :: DunbarCli m ()
requestOption = monad $ do
  friends :: Either String [(String, Friend)] <- F.retrieveAll
  case friends of
    (Left err) -> return $ output err stop
    (Right _) -> return $ output M.mainMenu readOption

viewFriends :: DunbarCli m ()
viewFriends = monad $ do
  friends <- F.retrieveAll
  case friends of
    (Left err) -> return $ output err stop
    (Right friends) -> return $ output (showFriends friends) requestOption
  where showTuple (i, r) = i ++ ": " ++ Friend.showName r
        showFriends [] = "<no-friends>"
        showFriends rs = unlines $ map showTuple $ sortOn fst rs

requestIdToDelete :: DunbarCli m ()
requestIdToDelete = output "Enter ID of friend to delete:" readIdToDelete

requestIdToShow :: DunbarCli m ()
requestIdToShow = output M.enterFriendId readIdToShow

readIdToDelete :: DunbarCli m ()
readIdToDelete = input $ \s -> deleteFriend s

readIdToShow :: DunbarCli m ()
readIdToShow = input $ \s -> showFriend s

showFriend :: (Cli m, F.Store m Friend) => String -> (DunbarCli m ())
showFriend i = monad $ do
  friend <- F.retrieve i
  case friend of
    (Left err) -> return $ output err stop
    (Right (Just friend)) -> return $ output (Friend.showName friend) requestOption
    (Right Nothing) -> return $ output (M.friendDoesNotExist i) requestOption


deleteFriend :: (Cli m, F.Store m Friend) => String -> DunbarCli m ()
deleteFriend s = monad $ do
  deleted <- F.delete s
  case deleted of
    (Right (Just friend)) -> return $ output ("Deleted: " ++ Friend.showName friend) requestOption
    (Right Nothing) -> return $ output ("Couldn't find friend with ID: " ++ s) requestOption
    (Left err) -> return $ output err requestOption

readOption :: DunbarCli m ()
readOption = input $ \s -> case (toLower . strip . pack $ s) of
  "n" -> newFriend
  "v" -> viewFriends
  "s" -> requestIdToShow
  "d" -> requestIdToDelete
  "q" -> stop
  _ -> output "Invalid Option: please try again" requestOption

enterValue :: (Cli m, F.Store m Friend) => String -> (String -> Either String a) -> Console m a
enterValue prompt reader = output prompt $
                           input $ \s -> case reader s of
                             (Right a) -> output ("You entered: " ++ s) (return a)
                             (Left err) -> output err (enterValue prompt reader)

notEmptyString :: String -> String -> Either String String
notEmptyString err "" = Left err
notEmptyString _ input = Right input

newFriend :: (Cli m, F.Store m Friend) => Console m ()
newFriend = do
  firstName <- enterValue M.enterFirstname (notEmptyString M.emptyFirstname)
  lastName  <- enterValue M.enterLastname (notEmptyString M.emptyLastname)
  note      <- enterValue M.enterNote return
  let friend = Friend.newFriend firstName lastName (notes note)
  result <- monad $ return <$> F.store friend
  case result of
    (Left err) -> output err requestOption
    (Right _) -> requestOption
  where notes "" = []
        notes n = [n]

start :: DunbarCli m ()
start = requestOption

main' :: String -> IO ()
main' s = runForFile s (runInputIO requestOption)

main :: IO ()
main = main' "resources.txt"
