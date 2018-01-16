{-# LANGUAGE RankNTypes #-}

module AppSpec (
  main
)
where

import App.DunbarCli (app)
import qualified App.Messages as M
import Utils.List (maybeHead)
import Test.Hspec (hspec, it, describe, shouldBe, Expectation)
import Data.Friend (Friend, newFriend, showName)
import qualified Data.Friend as Friend
import qualified Control.Monad.State as ST
import SpecUtils ((==>))
import Consolation.Spec (stdin, stdout, ExpectedIO, runExpectedIO, expectedOutput)

-- FIXME always have the initial state as empty - should be completely black box

cliFlow :: [(String, Friend)] -> ExpectedIO -> Expectation
cliFlow initState stdio = actualOutput ==> expectedOutput stdio
  where (actualOutput, finalState) = ST.runState (runExpectedIO app stdio) initState

createFriend :: String -> String -> String -> ExpectedIO
createFriend firstname lastname note =
  concat $
  [ stdout M.enterFirstname
  , stdin firstname
  , stdout ("You entered: " ++ firstname)

  , stdout M.enterLastname
  , stdin lastname
  , stdout ("You entered: " ++ lastname)

  , stdout M.enterNote
  , stdin note
  , stdout ("You entered: " ++ note)
  ]

main :: IO ()
main = hspec $ do
  describe "CLI (state monad)" $ do

    it "asks for user input when nothing supplied" $ do
      cliFlow [] $ stdout M.mainMenu

    it "prints <no-friends> when no friends have been stored" $ do
      cliFlow []  $ stdout M.mainMenu
                 ++ stdin "v"
                 ++ stdout "<empty>"
                 ++ stdout M.mainMenu

    it "pretty-prints any friends that have been stored" $ do
      let friends = [("0", (newFriend "Darth" "Maul" [])), ("1", (newFriend "Kylo" "Ren" []))]
      cliFlow friends  $ stdout M.mainMenu
                      ++ stdin "v"
                      ++ stdout "0: Darth Maul\n1: Kylo Ren\n"
                      ++ stdout M.mainMenu

    it "can save a new friend" $ do
      let friends = [("0", (newFriend "Darth" "Maul" []))]
      cliFlow friends    $ stdout M.mainMenu
                        ++ stdin "n"
                        ++ createFriend "Darth" "Vadar" "He is a bad man"
                        ++ stdout M.mainMenu
                        ++ stdin "v"
                        ++ stdout "0: Darth Maul\n1: Darth Vadar - He is a bad man\n"
                        ++ stdout M.mainMenu

    it "should re-ask user for input if first name or lastname is empty" $ do
      cliFlow []  $ stdout M.mainMenu
                 ++ stdin "n"
                 ++ stdout M.enterFirstname
                 ++ stdin ""
                 ++ stdout M.emptyFirstname
                 ++ stdout M.enterFirstname
                 ++ stdin "a"
                 ++ stdout "You entered: a"
                 ++ stdout M.enterLastname
                 ++ stdin ""
                 ++ stdout M.emptyLastname
                 ++ stdout M.enterLastname
                 ++ stdin "b"
                 ++ stdout "You entered: b"
                 ++ stdout M.enterNote
                 ++ stdin "blah"
                 ++ stdout "You entered: blah"
                 ++ stdout M.mainMenu

    it "can delete a friend" $ do
      let darth = newFriend "Darth" "Maul" []
          luke = newFriend "Luke" "Skywalker" []
          friends = [("0", darth), ("1", luke)]
      cliFlow friends $ concat $
                      [ stdout M.mainMenu
                      , stdin "s"
                      , stdout M.enterFriendId
                      , stdin "0"
                      , stdout (Friend.prettyPrint "0" darth)
                      , stdout M.friendMenu
                      , stdin "d"
                      , stdout "Deleted Darth Maul"
                      , stdout M.friendMenu
                      , stdin "q"
                      , stdout M.mainMenu
                      , stdin "v"
                      , stdout "1: Luke Skywalker\n"
                      , stdout M.mainMenu ]

    it "can show an individual friend" $ do
      let leia = (newFriend "Princess" "Leia" [])
          friends = [("0", leia)]
      cliFlow friends $ concat $
                      [ stdout M.mainMenu
                      , stdin "s"
                      , stdout M.enterFriendId
                      , stdin "0"
                      , stdout (Friend.prettyPrint "0" leia)
                      , stdout M.friendMenu
                      , stdin "q"
                      , stdout M.mainMenu ]

    it "errors appropriately if trying to show non-existant friend" $ do
      let friends = [("0", (newFriend "Princess" "Leia" []))]
      cliFlow friends $ concat $
                      [ stdout M.mainMenu
                      , stdin "s"
                      , stdout M.enterFriendId
                      , stdin "1"
                      , stdout (M.friendDoesNotExist "1")
                      , stdout M.mainMenu]

    it "doesn't process any more inputs if app has been quit" $ do
      cliFlow [] $ concat $
                 [ stdout M.mainMenu
                 , stdin "q"
                 , stdout M.goodbye
                 , stdin "n"
                 , stdin "hello"
                 , stdin "anyone there!!"]

    it "should support paging through friends, 10 per page" $ do
      let friendForLetter i = newFriend ("f" ++ (i:[])) ("l" ++ (i:[])) []
          generatedFriends = reverse $ zip (map (:[]) ['a'..]) $ map friendForLetter ['a'..'z']
          listView = unlines . map (\i -> (i:[]) ++ ": " ++ showName (friendForLetter i))
      cliFlow generatedFriends $ stdout M.mainMenu
                              ++ stdin "v"
                              ++ stdout (listView ['a'..'j'])
                              ++ stdout M.continuePaging
                              ++ stdin ""
                              ++ stdout (listView ['k'..'t'])
                              ++ stdout M.continuePaging
                              ++ stdin ""
                              ++ stdout (listView ['u'..'z'])
                              ++ stdout M.mainMenu

    it "can add note to friend" $ do
       cliFlow [] $ stdout M.mainMenu
                 ++ stdin "n"
                 ++ createFriend "Billy" "Ray" "Note 1"
                 ++ stdout M.mainMenu
                 ++ stdin "s"
                 ++ stdout M.enterFriendId
                 ++ stdin "0"
                 ++ stdout (Friend.prettyPrint "0" (newFriend "Billy" "Ray" ["Note 1"]))
                 ++ stdout M.friendMenu
                 ++ stdin "n"
                 ++ stdout M.addNote
                 ++ stdin "Note 2"
                 ++ stdout "You entered: Note 2"
                 ++ stdout (Friend.prettyPrint "0" (newFriend "Billy" "Ray" ["Note 1", "Note 2"]))
                 ++ stdout M.friendMenu
                 ++ stdin "q"
                 ++ stdout M.mainMenu
