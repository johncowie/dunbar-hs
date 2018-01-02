{-# LANGUAGE RankNTypes #-}

module AppSpec (
  main
)
where

import App.DunbarCli (start)
import Utils.Cli (runState, ConsoleStep, CliState)
import Utils.List (maybeHead)
import Test.Hspec (hspec, it, describe, shouldBe, Expectation)
import Data.Friend (Friend, newFriend)
import Control.Monad.State (State)
import SpecUtils ((==>))

-- TODO create utility functions for inspecting inputs and outputs in CliState
outputs :: (([String], [String]), x) -> [String]
outputs ((is, os), x) = os

-- TODO look and see if that CliState type alias can be improved
runApp :: [(String, Friend)] -> [String] -> ConsoleStep (CliState [(String, Friend)]) -> (([String], [String]), [(String, Friend)])
runApp initialState inputs startStep = runState initialState inputs start

output :: [(String, Friend)] -> [String] -> ConsoleStep (CliState [(String, Friend)]) -> [String]
output initialState inputs startStep = outputs (runState initialState inputs start)

mainMenu = "Choose option: (n)ew friends; (v)iew friends; (d)elete friend"

main :: IO ()
main = hspec $ do
  describe "CLI (state monad)" $ do

    it "asks for user input when nothing supplied" $ do
      output [] [] start ==> [mainMenu]

    it "prints <no-friends> when no friends have been stored" $ do
      (head $ reverse $ take 2 $ output [] ["v"] start) ==> "<no-friends>"

    it "pretty-prints any friends that have been stored" $ do
      let friends = [("0", (newFriend "Darth" "Maul")), ("1", (newFriend "Kylo" "Ren"))]
      (head $ reverse $ take 2 $ output friends ["v"] start) ==> "0: Darth Maul\n1: Kylo Ren\n"

    it "can save a new friend" $ do
      let friends = [("0", (newFriend "Darth" "Maul"))]
          inputs = ["n", "Darth", "Vadar", "v"]
      (head $ reverse $ take 2 $ output friends inputs start) ==> "0: Darth Maul\n1: Darth Vadar\n"

    it "can delete a friend" $ do
      let friends = [("0", (newFriend "Darth" "Maul")), ("1", (newFriend "Luke" "Skywalker"))]
          inputs = ["d", "0", "v"]
      (head $ reverse $ take 2 $ output friends inputs start) ==> "1: Luke Skywalker\n"
