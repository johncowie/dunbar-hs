{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.DunbarCli (
  start
) where

import qualified Utils.Cli as C
import qualified Store as F
import Store.File (runForFile)
import Data.Text (strip, toLower, pack)
import Data.Friend (Friend, newFriend, showName)
import Data.List (sortOn)
import qualified App.Messages as M

requestOption :: (F.Store m Friend) => C.ConsoleStep m
requestOption = C.mStep $ do
  friends :: Either String [(String, Friend)] <- F.retrieveAll
  case friends of
    (Left err) -> return $ C.withOutput err C.terminate
    (Right _) -> return $ C.withOutput M.mainMenu readOption

viewFriends :: (F.Store m Friend) => C.ConsoleStep m
viewFriends = C.mStep $ do
  friends <- F.retrieveAll
  case friends of
    (Left err) -> return $ C.withOutput err C.terminate
    (Right friends) -> return $ C.withOutput (showFriends friends) requestOption
  where showTuple (i, r) = i ++ ": " ++ showName r
        showFriends [] = "<no-friends>"
        showFriends rs = unlines $ map showTuple $ sortOn fst rs

requestIdToDelete :: (F.Store m Friend) => C.ConsoleStep m
requestIdToDelete = C.withOutput "Enter ID of friend to delete:" readIdToDelete

requestIdToShow :: (F.Store m Friend) => C.ConsoleStep m
requestIdToShow = C.withOutput M.enterFriendId readIdToShow

readIdToDelete :: (F.Store m Friend) => C.ConsoleStep m
readIdToDelete = C.inputStep $ \s -> C.mStep (deleteFriend s)

readIdToShow :: (F.Store m Friend) => C.ConsoleStep m
readIdToShow = C.inputStep $ \s -> C.mStep (showFriend s)

showFriend :: (F.Store m Friend) => String -> m (C.ConsoleStep m)
showFriend i = do
  friend <- F.retrieve i
  case friend of
    (Left err) -> return $ C.withOutput err C.terminate
    (Right (Just friend)) -> return $ C.withOutput (showName friend) requestOption
    (Right Nothing) -> return $ C.withOutput (M.friendDoesNotExist i) requestOption


deleteFriend :: (F.Store m Friend) => String -> m (C.ConsoleStep m)
deleteFriend s = do
  deleted <- F.delete s
  case deleted of
    (Right (Just friend)) -> return $ C.withOutput ("Deleted: " ++ showName friend) requestOption
    (Right Nothing) -> return $ C.withOutput ("Couldn't find friend with ID: " ++ s) requestOption
    (Left err) -> return $ C.withOutput err requestOption

readOption :: (F.Store m Friend) => C.ConsoleStep m
readOption = C.inputStep $ \s -> case (toLower . strip . pack $ s) of
  "n" -> requestFirstName
  "v" -> viewFriends
  "s" -> requestIdToShow
  "d" -> requestIdToDelete
  "q" -> C.terminate
  _ -> C.withOutput "Invalid Option: please try again" requestOption

requestFirstName :: (F.Store m Friend) => C.ConsoleStep m
requestFirstName = C.withOutput "Enter firstname" readFirstName

readFirstName :: (F.Store m Friend) => C.ConsoleStep m
readFirstName = C.inputStep $ \firstName -> do
  C.withOutput ("You entered: " ++ firstName) (requestLastName firstName)

requestLastName :: (F.Store m Friend) => String -> C.ConsoleStep m
requestLastName firstName = C.withOutput "Enter lastname" (readLastName firstName)

readLastName :: (F.Store m Friend) => String -> C.ConsoleStep m
readLastName firstName = C.inputStep $ \lastName -> do
  C.mStep (storeFriend (newFriend firstName lastName))

storeFriend :: (F.Store m Friend) => Friend -> m (C.ConsoleStep m)
storeFriend f = do
  result <- F.store f
  case result of
    (Left err) -> return $ C.withOutput err $ C.step requestOption
    (Right _) -> return requestOption

start :: (F.Store m Friend) => C.ConsoleStep m
start = requestOption

main' :: String -> IO ()
main' s = runForFile s (C.runInputIO requestOption)

main :: IO ()
main = main' "resources.txt"
