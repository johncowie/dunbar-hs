{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module App.DunbarCli (
  app
) where

import           Consolation.Console (Console, output, input, lift, stop, run)
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

type DunbarCli m a = (Cli m, F.Store m Friend) => Console m a

abort :: String -> Console m a
abort err = output err stop

returnWithOutput :: (Cli m) => String -> Console m ()
returnWithOutput s = output s (return ())

returnOrReroute :: DunbarCli m a -> m (Either String a) -> DunbarCli m a
returnOrReroute reroute e = either (flip output reroute) return =<< lift e

returnOrStop :: m (Either String a) -> DunbarCli m a
returnOrStop = returnOrReroute stop

viewFriends :: DunbarCli m [(String, Friend)]
viewFriends = do
  friends <- returnOrStop F.retrieveAll
  output (showRecords Friend.showName friends) (return friends)

showRecords :: (a -> String) -> [(String, a)] -> String
showRecords _ [] = "<empty>"
showRecords f xs = unlines $ map showTuple $ sortOn fst xs
  where showTuple (i, r) = i ++ ": " ++ f r

viewFriend :: DunbarCli m ()
viewFriend =
  output M.enterFriendId $
  input $ \s -> do
    friend <- lift $ F.retrieve s
    case friend of
      (Left err) -> abort err
      (Right (Just friend)) -> output (Friend.showName friend) (return ())
      (Right Nothing) -> output (M.friendDoesNotExist s) (return ())


deleteFriend :: DunbarCli m ()
deleteFriend =
  output M.enterDeleteFriendId $
  input $ \s -> do
  deleted <- lift $ F.delete s
  case deleted of
    (Right (Just friend)) -> returnWithOutput ("Deleted: " ++ Friend.showName friend)
    (Right Nothing) -> returnWithOutput ("Couldn't find friend with ID: " ++ s)
    (Left err) -> returnWithOutput err

mainMenu :: DunbarCli m ()
mainMenu = output M.mainMenu $
           input $ \s -> do
              case (toLower . strip . pack $ s) of
                "n" -> newFriend >> mainMenu
                "v" -> viewFriends >> mainMenu
                "s" -> viewFriend >> mainMenu
                "d" -> deleteFriend >> mainMenu
                "q" -> abort M.goodbye
                _ -> output "Invalid Option: please try again" mainMenu

enterValue :: (Cli m, F.Store m Friend) => String -> (String -> Either String a) -> Console m a
enterValue prompt reader = output prompt $
                           input $ \s -> case reader s of
                             (Right a) -> output ("You entered: " ++ s) (return a)
                             (Left err) -> output err (enterValue prompt reader)

notEmptyString :: String -> String -> Either String String
notEmptyString err "" = Left err
notEmptyString _ input = Right input

newFriend :: DunbarCli m ()
newFriend = do
  firstName <- enterValue M.enterFirstname (notEmptyString M.emptyFirstname)
  lastName  <- enterValue M.enterLastname (notEmptyString M.emptyLastname)
  note      <- enterValue M.enterNote return
  let friend = Friend.newFriend firstName lastName (notes note)
  returnOrReroute (return ()) $ F.store friend
  where notes "" = []
        notes n = [n]

app :: (Cli m, F.Store m Friend) => m ()
app = run mainMenu

main' :: String -> IO ()
main' s = runForFile s $ runInputT defaultSettings $ app

main :: IO ()
main = main' "resources.txt"
