{-# LANGUAGE RankNTypes #-}

module AppSpec (
  main
)
where

import App.DunbarCli (start)
import qualified App.Messages as M
import Utils.Cli (runState, ConsoleStep, CliState)
import Utils.List (maybeHead)
import Test.Hspec (hspec, it, describe, shouldBe, Expectation)
import Data.Friend (Friend, newFriend)
import Control.Monad.State (State)
import SpecUtils ((==>))

output :: [(String, Friend)] -> [String] -> ConsoleStep (CliState [(String, Friend)]) -> [String]
output initialState inputs startStep = outputs (runState initialState inputs start)
  where outputs ((is, os), x) = os

stdin :: String -> (String, Bool)
stdin s = (s, True)

stdout :: String -> (String, Bool)
stdout s = (s, False)

cliFlow :: [(String, Friend)] -> [(String, Bool)] -> Expectation
cliFlow initState stdio = (reverse $ output initState stdins start) ==> stdouts
  where stdins = map fst $ filter snd stdio
        stdouts = map fst $ filter (not . snd) stdio

main :: IO ()
main = hspec $ do
  describe "CLI (state monad)" $ do

    it "asks for user input when nothing supplied" $ do
      cliFlow [] [stdout M.mainMenu]

    it "prints <no-friends> when no friends have been stored" $ do
      cliFlow [] [  stdout M.mainMenu
                  , stdin "v"
                  , stdout "<no-friends>"
                  , stdout M.mainMenu ]

    it "pretty-prints any friends that have been stored" $ do
      let friends = [("0", (newFriend "Darth" "Maul" [])), ("1", (newFriend "Kylo" "Ren" []))]
      cliFlow friends [ stdout M.mainMenu
                      , stdin "v"
                      , stdout "0: Darth Maul\n1: Kylo Ren\n"
                      , stdout M.mainMenu ]

    it "can save a new friend" $ do
      let friends = [("0", (newFriend "Darth" "Maul" []))]
      cliFlow friends [ stdout M.mainMenu
                      , stdin "n"
                      , stdout M.enterFirstname
                      , stdin "Darth"
                      , stdout "You entered: Darth"
                      , stdout M.enterLastname
                      , stdin "Vadar"
                      , stdout "You entered: Vadar"
                      , stdout M.enterNote
                      , stdin "He is a bad man"
                      , stdout M.mainMenu
                      , stdin "v"
                      , stdout "0: Darth Maul\n1: Darth Vadar - He is a bad man\n"
                      , stdout M.mainMenu
                      ]

    it "should re-ask user for input if first name or lastname is empty" $ do
      cliFlow [] [ stdout M.mainMenu
                 , stdin "n"
                 , stdout M.enterFirstname
                 , stdin ""
                 , stdout M.emptyFirstname
                 , stdout M.enterFirstname
                 , stdin "a"
                 , stdout "You entered: a"
                 , stdout M.enterLastname
                 , stdin ""
                 , stdout M.emptyLastname
                 , stdout M.enterLastname
                 , stdin "b"
                 , stdout "You entered: b"
                 , stdout M.enterNote
                 , stdin "blah"
                 , stdout M.mainMenu
                 ]

    it "can delete a friend" $ do
      let friends = [("0", (newFriend "Darth" "Maul" [])), ("1", (newFriend "Luke" "Skywalker" []))]
      cliFlow friends [ stdout M.mainMenu
                      , stdin "d"
                      , stdout "Enter ID of friend to delete:"
                      , stdin "0"
                      , stdout "Deleted: Darth Maul"
                      , stdout M.mainMenu
                      , stdin "v"
                      , stdout "1: Luke Skywalker\n"
                      , stdout M.mainMenu ]

    it "can show an individual friend" $ do
      let friends = [("0", (newFriend "Princess" "Leia" []))]
      cliFlow friends [ stdout M.mainMenu
                      , stdin "s"
                      , stdout M.enterFriendId
                      , stdin "0"
                      , stdout "Princess Leia"
                      , stdout M.mainMenu]

    it "errors appropriately if trying to show non-existant friend" $ do
      let friends = [("0", (newFriend "Princess" "Leia" []))]
      cliFlow friends [ stdout M.mainMenu
                      , stdin "s"
                      , stdout M.enterFriendId
                      , stdin "1"
                      , stdout (M.friendDoesNotExist "1")
                      , stdout M.mainMenu]
