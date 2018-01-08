{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module App.DunbarCli (
  app
) where

import           Consolation.Console (Console, output, input, exit, run, continue)
import           System.Console.Haskeline (runInputT, defaultSettings)
import           Consolation.Cli (Cli)
import           Utils.Cli -- importing instances
import qualified Store as F
import           Store.File (runForFile)
import           Data.Text (strip, toLower, pack)
import           Data.Friend (Friend)
import qualified Data.Friend as Friend
import           Data.List (sortOn)
import qualified App.Messages as M
import           Control.Monad.Trans (lift)

type DunbarCli m a = (Cli m, F.Store m Friend) => Console m a

abort :: String -> DunbarCli m a
abort err = output err >> exit

returnOrStop :: DunbarCli m (Either String a) -> DunbarCli m a
returnOrStop e = either abort return =<< e

viewFriends :: DunbarCli m [(String, Friend)]
viewFriends = do
  friends <- returnOrStop (lift F.retrieveAll)
  output (showRecords Friend.showName friends)
  return friends

showRecords :: (a -> String) -> [(String, a)] -> String
showRecords _ [] = "<empty>"
showRecords f xs = unlines $ map showTuple $ sortOn fst xs
  where showTuple (i, r) = i ++ ": " ++ f r

viewFriend :: DunbarCli m ()
viewFriend = do
  output M.enterFriendId
  s <- input
  friend <- lift $ F.retrieve s
  case friend of
    (Left err) -> abort err
    (Right (Just friend)) -> output (Friend.showName friend)
    (Right Nothing) -> output (M.friendDoesNotExist s)


deleteFriend :: DunbarCli m ()
deleteFriend = do
  output M.enterDeleteFriendId
  s <- input
  deleted <- lift $ F.delete s
  case deleted of
    (Right (Just friend)) -> output ("Deleted: " ++ Friend.showName friend)
    (Right Nothing) -> output ("Couldn't find friend with ID: " ++ s)
    (Left err) -> output err

mainMenu :: DunbarCli m ()
mainMenu = do
  output M.mainMenu
  s <- input
  case (toLower . strip . pack $ s) of
    "n" -> newFriend >> mainMenu
    "v" -> viewFriends >> mainMenu
    "s" -> viewFriend >> mainMenu
    "d" -> deleteFriend >> mainMenu
    "q" -> abort M.goodbye
    _ -> output "Invalid Option: please try again" >> mainMenu

enterValue :: (Cli m, F.Store m Friend) => String -> (String -> Either String a) -> Console m a
enterValue prompt reader = do
  output prompt
  s <- input
  case reader s of
    (Right a) -> output ("You entered: " ++ s) >> return a
    (Left err) -> output err >> (enterValue prompt reader)

notEmptyString :: String -> String -> Either String String
notEmptyString err "" = Left err
notEmptyString _ input = Right input

newFriend :: DunbarCli m ()
newFriend = do
  firstName <- enterValue M.enterFirstname (notEmptyString M.emptyFirstname)
  lastName  <- enterValue M.enterLastname (notEmptyString M.emptyLastname)
  note      <- enterValue M.enterNote return
  let friend = Friend.newFriend firstName lastName (notes note)
  result <- lift $ F.store friend
  case result of
    (Left err) -> output err >> continue
    (Right r) -> continue
  where notes "" = []
        notes n = [n]

app :: (Cli m, F.Store m Friend) => Console m ()
app = mainMenu

main' :: String -> IO ()
main' s = runForFile s $ runInputT defaultSettings $ run app

main :: IO ()
main = main' "resources.txt"
