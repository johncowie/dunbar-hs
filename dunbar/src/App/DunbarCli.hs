{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module App.DunbarCli (
  start
) where

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

abort :: String -> Console m a
abort err = output err stop

returnOrReroute :: DunbarCli m a -> m (Either String a) -> DunbarCli m a
returnOrReroute reroute mValue = monad $ either (flip output reroute) return <$> mValue

returnOrStop :: m (Either String a) -> DunbarCli m a
returnOrStop = returnOrReroute stop

viewFriends :: DunbarCli m [(String, Friend)]
viewFriends = do
  friends <- returnOrStop F.retrieveAll
  output (showFriends friends) (return friends)
  where showTuple (i, r) = i ++ ": " ++ Friend.showName r
        showFriends [] = "<no-friends>"
        showFriends rs = unlines $ map showTuple $ sortOn fst rs

showFriend :: DunbarCli m ()
showFriend =
  output M.enterFriendId $
  input $ \s -> do
  monad $ do
  friend <- F.retrieve s
  case friend of
    (Left err) -> return $ output err stop
    (Right (Just friend)) -> return $ output (Friend.showName friend) (return ())
    (Right Nothing) -> return $ output (M.friendDoesNotExist s) (return ())


deleteFriend :: DunbarCli m ()
deleteFriend =
  output "Enter ID of friend to delete:" $
  input $ \s -> do
  monad $ do
  deleted <- F.delete s
  case deleted of
    (Right (Just friend)) -> return $ output ("Deleted: " ++ Friend.showName friend) (return ())
    (Right Nothing) -> return $ output ("Couldn't find friend with ID: " ++ s) (return ())
    (Left err) -> return $ output err (return ())

mainMenu :: DunbarCli m ()
mainMenu = output M.mainMenu $
           input $ \s -> do
              case (toLower . strip . pack $ s) of
                "n" -> newFriend >> mainMenu
                "v" -> viewFriends >> mainMenu
                "s" -> showFriend >> mainMenu
                "d" -> deleteFriend >> mainMenu
                "q" -> stop
                _ -> output "Invalid Option: please try again" mainMenu

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
  returnOrReroute (return ()) $ F.store friend
  where notes "" = []
        notes n = [n]

start :: DunbarCli m ()
start = mainMenu

main' :: String -> IO ()
main' s = runForFile s (runInputIO mainMenu)

main :: IO ()
main = main' "resources.txt"
