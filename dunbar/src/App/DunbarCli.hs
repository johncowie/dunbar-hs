{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module App.DunbarCli (
  start
) where

import qualified Utils.Cli as C
import qualified Store as F
import Store.File (runForFile)
import Data.Text (strip, toLower, pack)
import Data.Friend (Friend, newFriend, showName)
import Data.List (sortOn)
import Control.Monad.State (State, StateT, runStateT, runState)

requestOption :: (F.Store m Friend) => C.ConsoleStep m
requestOption = C.withOutput "Choose option: (n)ew friends; (v)iew friends;" $ C.inputStep readOption

viewFriends :: (F.Store m Friend) => m (C.ConsoleStep m)
viewFriends = do
  friends <- F.retrieveAll
  case friends of
    (Left err) -> return $ C.withOutput err $ C.step requestOption
    (Right friends) -> return $ C.withOutput (showFriends friends) $ C.step requestOption
  where showTuple (i, r) = i ++ ": " ++ showName r
        showFriends [] = "<no-friends>"
        showFriends rs = unlines $ map showTuple $ sortOn fst rs

requestIdToDelete :: (F.Store m Friend) => C.ConsoleStep m
requestIdToDelete = C.withOutput "Enter ID of friend to delete:" $ C.inputStep readIdToDelete

readIdToDelete :: (F.Store m Friend) => String -> C.ConsoleStep m
readIdToDelete = undefined

readOption :: (F.Store m Friend) => String -> C.ConsoleStep m
readOption s = case (toLower . strip . pack $ s) of
  "n" -> requestFirstName
  "v" -> C.mStep viewFriends
  -- "d" -> requestIdToDelete
  "q" -> C.terminate
  _ -> C.withOutput "Invalid Option: please try again" $ C.step requestOption

requestFirstName :: (F.Store m Friend) => C.ConsoleStep m
requestFirstName = C.withOutput "Enter firstname" $ C.inputStep readFirstName

readFirstName :: (F.Store m Friend) => String -> C.ConsoleStep m
readFirstName firstName = C.withOutput ("You entered: " ++ firstName) $ C.step (requestLastName firstName)

requestLastName :: (F.Store m Friend) => String -> C.ConsoleStep m
requestLastName firstName = C.withOutput "Enter lastname" $ C.inputStep (readLastName firstName)

readLastName :: (F.Store m Friend) => String -> String -> C.ConsoleStep m
readLastName firstName lastName = C.mStep (storeFriend (newFriend firstName lastName))

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
main = runForFile "resources.txt" (C.runInputIO requestOption)
